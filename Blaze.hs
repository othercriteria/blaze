{-# LANGUAGE TypeSynonymInstances #-}

module Blaze
    where

-- Immutable tree data structure with fairly easy/cheap updates
import Tree
      
import Data.Number.LogFloat hiding (log)
import Data.List
import qualified Numeric.GSL.Special.Gamma as Gamma
import Text.Printf
import Control.Monad.Random

-- Renaming GSL imports
betaDensity = Gamma.beta

-- Expose a function for tagging nodes
tag :: Tree a -> String -> Tree a
tag = tagNode

-- Tag addresses give a more natural way of specifying a location in a
-- graph.

type TA = TagAddress

getTA :: TA -> Tree a -> Tree a
getTA ta t = getAt (tagsToAddress t ta) t

-- Transparently represent probabilities in log-space to
-- avoid underflows.

type Prob = LogFloat

zeroProb :: Prob
zeroProb = logFloat (0.0 :: Double)

oneProb :: Prob
oneProb = logFloat (1.0 :: Double)

oneOverProb :: Double -> Prob
oneOverProb = logFloat . (1 /)

-- Machine for inference

data Machine = Machine { ms::State
                       , md::Density
                       , mk::Kernel
                       , me::Entropy
                       }

instance Show Machine where
    show (Machine s d k e) = printf "Machine S:%s D:%s K:%s E:%s" s' d' k' e'
        where s' = show s
              d' = show d
              k' = show k
              e' = (init $ show (take 10 e)) ++ ",...]"

-- Structure to represent State tree

type State = Tree SNode

data SNode = SNode  { ssDEdges::[TA], value::Datum }
           | CSNode { ssDEdges::[TA]               }
             deriving (Eq)

instance Show SNode where
    show (SNode  sss v) = printf "(State: %s [%d des])" (show v) (length sss)
    show (CSNode sss  ) = printf "(CollectionState: [%d des])" (length sss)

data Datum = IntDatum    { iv::Int }
           | DoubleDatum { dv::Double }
           | StringDatum { sv::String }
           | NullDatum
             deriving (Eq)

instance Show Datum where
    show (IntDatum    i) = printf "Int %d" i
    show (DoubleDatum d) = printf "Double %.3f" d
    show (StringDatum s) = printf "String \"%s\"" s
    show (NullDatum    ) = "Null"

datum :: State -> Datum
datum = value . tvalue
                           
intVal :: State -> Int
intVal = iv . datum

doubleVal :: State -> Double
doubleVal = dv . datum

stringVal :: State -> String
stringVal = sv . datum
            
setVal :: State -> Datum -> State
setVal s dat = s { tvalue = (tvalue s) { value = dat } }

depEdges :: State -> [TA]
depEdges = ssDEdges . tvalue

-- Structure to represent Density tree

type Density = Tree DNode

data DNode = DNode   { likelihood::Likelihood,  dsData::[TA],  dsOthers::[TA] }
           | ACDNode { acdHead::TA,                           acdOthers::[TA] }
               
instance Show DNode where
    show (DNode   l d o) = printf "(Density d: %s o: %s)" (show d) (show o) 
    show (ACDNode h   o) =
        printf "(CDensity for %s, o: %s)" (showTags h) (show o)
                                    
instance Eq DNode where
    (DNode   _  d1 o1) == (DNode   _  d2 o2) = d1 == d2 && o1 == o2
    (ACDNode h1    o1) == (ACDNode h2    o2) = h1 == h2 && o1 == o2
    _                  == _                  = False

type Likelihood = State -> Density -> Prob

addDSOthers :: Density -> [TA] -> Density
addDSOthers d tas = d { tvalue = dv { dsOthers = tas ++ (dsOthers dv) } }
    where dv = tvalue d

gather :: State -> State -> [TA] -> State
gather s cs os = cs `collectStates` [dsos,deps]
    where dsos = collectStates dummyState (map (flip getTA s) os)
                 `tagNode` "dso"
          deps = collectStates dummyState (map (flip getTA s) (depEdges cs))
                 `tagNode` "dep"

ungather :: State -> State
ungather s = s `removeChild` dso `removeChild` dep
    where dso = getTA ["dso"] s
          dep = getTA ["dep"] s

-- Calculates the likelihood of the State under some Density.                
density :: Likelihood
density s d@(Node (DNode   l _  _ ) _    _) = l s d
density s   (Node (ACDNode h    os) [cd] _) = product $ map ascd shcs
    where shcs   = children $ getTA h s

          ascd c = density (gather s c os) cd

-- Calculates the global likelihood.
topDensity :: Machine -> Prob
topDensity m = density (ms m) (md m)

-- Structure to represent Kernel tree

-- Varieties of Kernels:

-- GP: Gaussian Perturbation, parametrized by scale
-- DP: Discrete Perturbation, parametrized by possible values
-- RDSL: Reassign Datapoint, Single Link
-- RDDL: Reassign Datapoint, Dual Link
-- CC: Concrete Cycle
-- CM: Concrete Mixture
-- CH: Conditional Hybrid
-- I: Identity
-- MH: Metropolis-Hastings
-- EG: Enumerative Gibbs
-- VC: Virtual Cycle
-- VM: Virtual Mixture
-- ERR: Error
-- URW: Unweighted Random Walk
-- WRW: Weighted Random Walk

type Kernel = Tree KNode

data KNode = GP Double TA
           | DP TA TA
           | RDSL TA
           | RDDL TA Tag Tag
           | CC
           | CM [Double]
           | CH TA (State -> Tag)
           | I
           | MH
           | EG
           | VC TA TA
           | VM TA TA
           | ERR String
           | URW [TA]
           | WRW [TA]

instance Eq KNode where
    (GP s1 ts1) == (GP s2 ts2) = (s1 == s2 && ts1 == ts2)
    (DP dta1 sta1) == (DP dta2 sta2) = (dta1 == dta2 && sta1 == sta2)
    (RDSL ts1) == (RDSL ts2) = (ts1 == ts2)
    (RDDL ts1 t1 t1') == (RDDL ts2 t2 t2') =
        (ts1 == ts2 && t1 == t2 && t1' == t2')
    CC == CC = True
    (CM ws1) == (CM ws2) = (ws1 == ws2)
    (CH ts1 _) == (CH ts2 _) = (ts1 == ts2)
    I == I = True
    MH == MH = True
    EG == EG = True
    (VC sa1 da1) == (VC sa2 da2) = (sa1 == sa2 && da1 == da2)
    (VM sa1 da1) == (VM sa2 da2) = (sa1 == sa2 && da1 == da2)
    (ERR s1) == (ERR s2) = (s1 == s2)
    (URW fs1) == (URW fs2) = (fs1 == fs2)
    (WRW fs1) == (WRW fs2) = (fs1 == fs2)
                      
instance Show KNode where
    show (GP scale tags) = printf "GP: scale = %f at %s" scale (showTags tags)
    show (DP dta sta) = printf "DP: set %s at %s" (showTags dta) (showTags sta)
    show (RDSL tags) = printf "RDSL: coll. %s" (showTags tags)
    show (RDDL ta t1 t2) =
        printf "RDDL: coll. %s: {%s} -> {%s}" (showTags ta) t1 t2
    show CC = "CCl"
    show (CM ws) = printf "CM: weights [%s]" (intercalate "," $ map show ws)
    show (CH tags _) = printf "CH on state %s" (showTags tags)
    show I = "No action"
    show MH = "Metropolis-Hastings adaptor"
    show EG = "Enumerative Gibbs sampling adaptor"
    show (VC sa da) = printf "VC: S: %s D: %s" (showTags sa) (showTags da)
    show (VM sa da) = printf "VM: S: %s D: %s" (showTags sa) (showTags da)
    show (ERR s) = printf "Error: %s" s
    show (URW fs) = printf "Unweighted RW terminals: [%s]" $
                        (intercalate "," $ map showTags fs)
    show (WRW fs) = printf "Weighted RW terminals: [%s]" $
                        (intercalate "," $ map showTags fs)

showTags :: TA -> String
showTags ts = "[{" ++ intercalate "},{" (map show ts) ++ "}]"

-- Representation of SampleNextMachine, SampleNextMove, and
-- EnumeratePossibleMoves operations.
              
class Samplable a where
    sampMach :: a -> Machine
    sampMove :: a -> Move
    enumMove :: a -> [Move]
                              
data Move = Move { target::Machine
                 , forwardDens::Prob
                 , reverseDens::Prob
                 , jacobian::Double
                 }
            deriving (Show)

-- This only makes sense when m' has been generated starting with m
joinTo :: Move -> Move -> Move
joinTo m m' = Move t f r j
    where t = target m'
          f = forwardDens m * forwardDens m'
          r = reverseDens m * reverseDens m'
          j = jacobian m * jacobian m'

-- Implementation of Kernels

-- Functions for deterministically making a random choice by consuming
-- Entropy.

pick :: Double -> [a] -> a
pick e xs = pickw e (repeat (1 / (fromIntegral $ length xs))) xs

pickw :: Double -> [Double] -> [a] -> a
pickw _ _      []                 = error "pickw: nothing to choose from"
pickw _ _      [x]                = x
pickw e (w:ws) (x:xs) | e < w     = x
                      | otherwise = pickw (e - w) ws xs

toWeights :: [Int] -> [Double]
toWeights is = map (/ (sum is')) is'
    where is' = map fromIntegral is
                                                 
instance Samplable Machine where
    sampMach (Machine s d k@(Node (RDDL ta t1 t2) [] _) (e:es)) =
        Machine news' d k es
        where cra    = tagsToAddress s ta
              cr     = getAt cra s

              srccc  = untaggedChild . getTagged t1 $ children cr
              srcx   = pick e $ children srccc
                     
              tarc   = getTagged t2 $ children cr
              tarcca = cra ++ address cr [ (== tarc), not . isTagged ]

              news   = addAt s tarcca srcx

              cra'   = tagsToAddress news ta
              cr'    = getAt cra' news
              srcc'  = getTagged t1 $ children cr'
              srcxa' = cra' ++
                       address cr' [ (== srcc'), not . isTagged, (== srcx) ]
              
              news'  = removeAt news srcxa'

    sampMach m@(Machine _ _   (Node (CM []) [] _) _     ) = m
    sampMach   (Machine s d k@(Node (CM ws) ks _) (e:es)) =
        (sampMach (Machine s d (pickw e ws ks) es)) { mk = k }

    sampMach m@(Machine _ _   (Node CC []       _) _) = m
    sampMach   (Machine s d k@(Node CC (kc:kcs) _) e) = rest { mk = k }
        where this = sampMach (Machine s d kc e)
              rest = sampMach (this { mk = mkCCKernel kcs })
              
    sampMach (Machine s d k@(Node (CH ts pred) cs _) e) =
        (sampMach (Machine s d c e)) { mk = k }
            where ct = pred (getTA ts s)
                  c  = getTagged ct cs

    sampMach m@(Machine _ _ (Node I _ _) _) = m

    sampMach m@(Machine s d k@(Node MH [c] _) (e:es)) =
        if (e < fromLogFloat pAcc)
        then (Machine s' d k e')
        else (Machine s  d k e')
            where (Move tar pFrw pRev jac) = sampMove (Machine s d c es)
                  (Machine s' d' k' e')    = tar

                  dPre = topDensity m
                  dPst = topDensity tar

                  pAcc = min 1 ((dPst / dPre) * (pRev / pFrw) * (logFloat jac))

    sampMach m@(Machine s d k@(Node (VM sa da) [ck] _) (e:es)) =
        if (null shcs) then m else (Machine s'' d k e')
            where (Node (ACDNode _ os) [cd] _) = getTA da d

                  sha  = tagsToAddress s sa
                  sh   = getAt sha s
                  shcs = children sh
                  c    = pick e shcs
                  subs = gather s c os
                         
                  (Machine s' d' k' e') = sampMach (Machine subs cd ck es)

                  s'' = update s sha c (ungather s')
                         
    sampMach m@(Machine s d k@(Node (VC sa da) [ck] _) e) =
        foldl appk m shcs
            where (Node (ACDNode _ os) [cd] _) = getTA da d

                  shcs = children $ getTA sa s
                  
                  appk (Machine s' _ _ e') c' = Machine (apps s' e' c') d k e'
                  apps s' e' c' = update s' (tagsToAddress s' sa) c' $
                                  ungather . ms . sampMach $
                                  (Machine (gather s' c' os) cd ck e')

    sampMach m@(Machine s d k@(Node (URW ftas) [] _) es) =
        Machine s' d k (drop nes es)
            where walk (s,_) e = pick e $ zip
                                 (children $ steps s)
                                 (map doubleVal $ children $ probs s)
                  ss           = scanl walk (s,1.0) es


                  code  = getTA ["code"]
                  steps = getTA ["steps"]
                  probs = getTA ["probs"]
                                
                  finish         = map (datum . flip getTA s) ftas
                  isdone x       = (datum . code . fst $ x) `elem` finish
                  (sp',(sp'':_)) = break isdone ss
                  nes            = length sp' + 1
                  (ss'',pp'')    = sp''

                  p' = mkDoubleData (product (pp'' : map snd sp')) `tag` "prob"
                  s' = update s [] (getTA ["trace"] s) $
                       ((ss'' `addChild` p') `tag` "trace")
                                  
    sampMach m@(Machine s d k e) = (target $ sampMove m) { mk = k }

    sampMove (Machine s d k@(Node (GP scale []) [] _) (e1:e2:es)) =
        Move (Machine news d k es) dens dens 1.0
            where norm = sqrt ((-2) * log e1) * cos (2 * pi * e2)
                  dens = normalDens norm 0.0 1.0
                  news = setVal s (DoubleDatum $ doubleVal s + scale * norm)
    sampMove (Machine s d k@(Node (GP scale ts) [] _) (e1:e2:es)) =
        Move (Machine news d k es) dens dens 1.0
            where oldsa = tagsToAddress s ts
                  oldpa = init oldsa
                  oldsd = getAt oldsa s
                  norm  = sqrt ((-2) * log e1) * cos (2 * pi * e2)
                  dens  = normalDens norm 0.0 1.0
                  newv  = doubleVal oldsd + scale * norm
                  newsd = mkDoubleParam (last ts) newv
                  news  = update s oldpa oldsd newsd

    sampMove (Machine s d k@(Node (DP dta [] ) [] _) (e:es)) =
        Move (Machine news d k es) dens dens 1.0
            where datums = map datum . children $ getTA dta s
                  dens   = oneOverProb $ (fromIntegral . length $ datums)
                  news   = setVal s (pick e datums)
    sampMove (Machine s d k@(Node (DP dta sta) [] _) (e:es)) =
        Move (Machine news d k es) dens dens 1.0
            where datums = map datum . children $ getTA dta s
                  oldsa  = tagsToAddress s sta
                  oldpa  = init oldsa
                  oldsd  = getAt oldsa s
                  dens   = oneOverProb $ (fromIntegral . length $ datums)
                  newsd  = setVal oldsd (pick e datums)
                  news   = update s oldpa oldsd newsd
                          
    sampMove (Machine s d k@(Node (RDSL h) [] tag) (e1:e2:es)) =
        Move (tar { mk = k }) dens dens 1.0
            where tar = sampMach $
                        (Machine s d (Node (RDDL h srct dest) [] tag) es)

                  sh      = getTA h s
                  (ts,ns) = unzip . summarizeComps $ sh
                           
                  srct = pickw e1 (toWeights ns) ts
                  dest = pick  e2 ts

                  dens = oneOverProb $ fromIntegral (length ns * sum ns)

    sampMove (Machine s d k@(Node (CM ws) cs _) (e:es)) =
        Move (tar { mk = k }) (logFloat w * fd) (logFloat w * rd) jac
            where (c,w)  = pickw e ws (zip cs ws)

                  (Move tar fd rd jac) = sampMove (Machine s d c es)
                  
    sampMove (Machine s d k@(Node CC [c]    _) e) =
        Move (tar { mk = k }) fd rd jac
            where (Move tar fd rd jac) = sampMove (Machine s d c e)
    sampMove (Machine s d k@(Node CC (c:cs) _) e) =
        Move (tar { mk = k }) fd rd jac
            where this = sampMove (Machine s d c e)
                  rest = sampMove ((target this) { mk = mkCCKernel cs })

                  (Move tar fd rd jac) = this `joinTo` rest
                         
    sampMove m@(Machine _ _ (Node I [] _) _) = Move m oneProb oneProb 1.0

    sampMove m@(Machine s d k@(Node (VC sa da) [ck] _) e) =
        foldl appm (Move m oneProb oneProb 1.0) shcs
            where (Node (ACDNode _ os) [cd] _) = getTA da d

                  shcs = children $ getTA sa s

                  appm move'@(Move m' _ _ _) c' =
                      move' `joinTo` Move (appk m' c') oneProb oneProb 1.0
                  appk (Machine s' _ _ e') c' = Machine (apps s' e' c') d k e'
                  apps s' e' c' = update s' (tagsToAddress s' sa) c' $
                                  ungather . ms . sampMach $
                                  (Machine (gather s' c' os) cd ck e')

    sampMove m@(Machine s d k@(Node (VM sa da) [ck] _) (e:es)) =
        if (null shcs)
        then Move m oneProb oneProb 1.0
        else Move (Machine s'' d k e') (cdens * fd) (cdens * rd) jac
            where (Node (ACDNode _ os) [cd] _) = d

                  sha   = tagsToAddress s sa
                  sh    = getAt sha s
                  shcs  = children sh
                  c     = pick e shcs
                  subs  = gather s c os
                  cdens = oneOverProb $ fromIntegral (length shcs)

                  (Move tar fd rd jac) = sampMove (Machine subs cd ck es)

                  (Machine s' d' k' e') = tar

                  s'' = update s sha c (ungather s')

    sampMove (Machine s d k (e:es)) = Move (tar { mk = k }) fd rd jac
            where moves = enumMove (Machine s d k es)
                  ws    = map (fromLogFloat . forwardDens) moves

                  (Move tar fd rd jac) = pickw e ws moves

    enumMove (Machine s d k@(Node (DP dta [] ) [] _) e) =
        [ Move (Machine news d k e) dens dens 1.0 | news <- newss ]
            where datums = map datum . children $ getTA dta s
                  dens   = oneOverProb $ (fromIntegral . length $ datums)
                  newss  = map (setVal s) datums
    enumMove (Machine s d k@(Node (DP dta sta) [] _) e) =
        [ Move (Machine news d k e) dens dens 1.0 | news <- newss ]
            where datums = map datum . children $ getTA dta s
                  oldsa  = tagsToAddress s sta
                  oldpa  = init oldsa
                  oldsd  = getAt oldsa s
                  dens   = oneOverProb $ (fromIntegral . length $ datums)
                  newsds = map (setVal oldsd) datums
                  newss  = map (update s oldpa oldsd) newsds
                          
    enumMove (Machine s d k@(Node (RDSL tags) [] _) e) =
        [ Move (Machine news' d k e) dens dens 1.0 | news' <- newss' ]
            where cra  = tagsToAddress s tags
                  cr   = getAt cra s
                  crcs = children cr

                  ctags  = map tagged crcs
                  cc t   = children . untaggedChild . getTagged t $ crcs
                  srcxts = concatMap (\t -> zip (cc t) (repeat t)) ctags

                  tarcca tarc = cra ++ address cr [ (== tarc), not . isTagged ]
                  tarccas     = map tarcca crcs

                  newsts = [ (addAt s tar srcx, srct) |
                             tar <- tarccas, (srcx, srct) <- srcxts ]
                  newss  = map fst newsts

                  cra'   = zipWith tagsToAddress newss (repeat tags)
                  cr'    = zipWith getAt cra' newss
                  crcs'  = map children cr'
                  srcc'  = zipWith getTagged (map snd srcxts) crcs'
                  mkPath = (\a b -> [ (== a), not . isTagged, (== b) ])
                  path   = zipWith mkPath srcc' (map fst srcxts)
                  srcxa' = zipWith3 (\a b c -> a ++ address b c) cra' cr' path

                  newss' = zipWith removeAt newss srcxa'

                  dens = oneOverProb $ fromIntegral (length newss)

    enumMove (Machine s d k@(Node CC [c]    _) e) =
        [ Move (tar { mk = k }) fd rd jac | (Move tar fd rd jac) <- moves ]
            where moves = enumMove (Machine s d c e)
    enumMove (Machine s d k@(Node CC (c:cs) _) e) =
        [ Move (tar { mk = k }) fd rd jac | (Move tar fd rd jac) <- moves ]
            where these  = enumMove (Machine s d c e)
                  rest t = enumMove ((target t) { mk = mkCCKernel cs })

                  restFrom t = [ t `joinTo` r | r <- rest t ]
                  moves      = concatMap restFrom these
                         
    enumMove (Machine s d k@(Node EG [c] _) e) =
        [ Move (tar { mk = k }) fd rd jac | (Move tar fd rd jac) <- moves' ]
            where moves = enumMove (Machine s d c e)
                  tars  = map target moves
                          
                  denss  = map topDensity tars
                  denss' = map (/ (sum denss)) denss

                  rmovess = map (\t -> enumMove (t { mk = k })) tars
                  rtarss  = map (map target) rmovess
                  rdensss = map (map topDensity) rtarss
                  hitms   = zipWith (\a b -> filter ((== (ms a)) . ms) b)
                            tars rtarss
                  hdensss = map (map topDensity) hitms

                  mkMove t d hd rd = Move t d (sum hd / sum rd) 1.0
                  moves'           = zipWith4 mkMove tars denss' hdensss rdensss

    enumMove m@(Machine s d (Node (ERR err) _ _) _) =
        error $ printf "ERROR Kernel encountered: %s\n\n%s" err (show m)
                                     
    enumMove (Machine s d k e) =
        error $ printf "Cannot enumerate %s" (show k)

collectKernels :: Kernel -> [Kernel] -> Kernel
collectKernels k ks = foldl (\c k' -> addAt c [] k') k ks
                             
mkGPKernel :: Double -> TA -> Kernel
mkGPKernel scale tags = mkNode $ GP scale tags

mkDPKernel :: TA -> TA -> Kernel
mkDPKernel dta sta = mkNode $ (DP dta sta)
                            
mkRDSLKernel :: TA -> Kernel
mkRDSLKernel tags = mkNode $ RDSL tags

mkRDDLKernel :: TA -> Tag -> Tag -> Kernel
mkRDDLKernel tags tag1 tag2 = mkNode $ RDDL tags tag1 tag2

mkCCKernel :: [Kernel] -> Kernel
mkCCKernel ks = collectKernels (mkNode CC) (reverse ks)

-- Kernels reversed as the fold in collectKernels builds the list backwards
mkCMKernel :: [Double] -> [Kernel] -> Kernel
mkCMKernel ws ks = collectKernels (mkNode $ CM ws) (reverse ks)

mkCHKernel :: TA -> (State -> Tag) -> [Kernel] -> Kernel
mkCHKernel tags pred ks = collectKernels (mkNode $ CH tags pred) ks

mkIKernel :: Kernel
mkIKernel = mkNode I

mkMHKernel :: Kernel -> Kernel
mkMHKernel k = mkNode MH `addChild` k

mkEGKernel :: Kernel -> Kernel
mkEGKernel k = mkNode EG `addChild` k

mkVCKernel :: TA -> TA -> Kernel -> Kernel
mkVCKernel sa da k = mkNode (VC sa da) `addChild` k
               
mkVMKernel :: TA -> TA -> Kernel -> Kernel
mkVMKernel sa da k = mkNode (VM sa da) `addChild` k

mkERRKernel :: String -> Kernel
mkERRKernel s = mkNode (ERR s)

mkURWKernel :: [TA] -> Kernel
mkURWKernel ftas = mkNode (URW ftas)
                
-- Structure to represent entropy source: these should be uniformly
-- distributed draws from the interval [0,1].

type Entropy = [Double]
                             
-- Methods for building States

mkDoubleData :: Double -> State
mkDoubleData d = mkNode $ SNode [] (DoubleDatum d)

mkDoubleParam :: Tag -> Double -> State
mkDoubleParam t d = mkDoubleData d `tagNode` t

mkDoubleParams :: TA -> [Double] -> [State]
mkDoubleParams = zipWith mkDoubleParam
                    
mkIntData :: Int -> State
mkIntData i = mkNode $ SNode [] (IntDatum i)

mkIntParam :: Tag -> Int -> State
mkIntParam t i = mkIntData i `tagNode` t

mkIntParams :: TA -> [Int] -> [State]
mkIntParams = zipWith mkIntParam

mkStringData :: String -> State
mkStringData s = mkNode $ SNode [] (StringDatum s)
              
mkTaggedData :: Tag -> Datum -> State
mkTaggedData t d = (mkNode $ SNode [] d) `tagNode` t

dummyState :: State
dummyState = mkNode $ SNode [] NullDatum

dummyCState :: State
dummyCState = mkNode $ CSNode []
             
collectStates :: State -> [State] -> State
collectStates c ss = foldl (\x s -> addAt x [] s) c ss

addDEdges :: State -> [TA] -> State
addDEdges s tas = s { tvalue = sv { ssDEdges = tas ++ (ssDEdges sv) } }
    where sv = tvalue s

-- Components in the sense of figure 3-10
isCompEmpty :: State -> Bool
isCompEmpty = (== 0) . length . children . untaggedChild

numInComp :: Tag -> State -> Int
numInComp t s = length . children . untaggedChild $
                getTagged t . children $ s

summarizeComps :: State -> [(Tag,Int)]
summarizeComps s = sort . map nc . children $ s
    where nc c = (tagged c, numInComp (tagged c) s)
                     
-- Useful likelihood calculations
            
getDSData :: State -> Density -> [State]
getDSData s d = map (flip getTA s) (dsData . tvalue $ d)

getDSOthers :: State -> Density -> [State]
getDSOthers s d = map (flip getTA s) (dsOthers . tvalue $ d)

getDeps :: State -> [State]
getDeps s = children $ getTagAt "dep" [] s

-- Densities
            
-- It is critical that this short-circuits!                  
multiplicative :: Likelihood
multiplicative s d | (null $ children d)   = oneProb
                   | (dens cd == zeroProb) = zeroProb
                   | otherwise             = dens cd * multiplicative s d'
                   where dens     = density s
                         (cd:cds) = children d
                         d'       = d `removeChild` cd

normalDens :: Double -> Double -> Double -> Prob
normalDens x mu sigma = logFloat $
                        (1 / sqrt (2 * pi * sigma ** 2)) *
                        exp (-((x - mu) ** 2) / (2 * sigma ** 2))

lognormalDens :: Double -> Double -> Double -> Prob
lognormalDens x mu sigma = logFloat $
                           (1 / (x * sigma * sqrt (2 * pi))) *
                           exp (-((log x - mu) ** 2) / (2 * sigma**2))
                            
-- j:    DoubleParam (interaction strength)
-- h:    DoubleParam (potential strength)
-- temp: DoubleParam (temperature)
-- spin: DoubleParam children (possible spins)
ising :: Likelihood
ising s d | (t <= 0)  = zeroProb
          | otherwise = logFloat $ max 0 (p x / sum (map p spins))
          where x       = doubleVal . head $ getDSData s d

                psts    = getDSOthers s d
                [j,h,t] = map (doubleVal . flip getTagged psts) ["j","h","temp"]
                spins   = map doubleVal . children $ getTagged "spin" psts

                nsum    = sum . map doubleVal $ getDeps s

                e x'    = ((-1)/2) * j * x' * nsum - h * x'
                p x'    = exp (-(e x') / t)
         
-- alpha: DoubleParam
-- beta:  DoubleParam
beta :: Likelihood
beta s d = logFloat $ like * norm
    where psts  = getDSOthers s d
          [a,b] = map (doubleVal . flip getTagged psts) ["alpha","beta"]
          x     = doubleVal . head $ getDSData s d
          norm  = 1 / betaDensity a b
          like  | (x < 0.0 || x > 1.0) = 0
                | otherwise            = (x ** (a - 1)) * ((1 - x) ** (b - 1))

-- p: DoubleParam
bernoulli :: Likelihood
bernoulli s d = logFloat $ like
    where p = doubleVal . getTagged "p" $ getDSOthers s d
          x = intVal . head $ getDSData s d

          like | (p < 0.0 || p > 1.0) = 0
               | (x == 0)             = (1 - p)
               | (x == 1)             = p

-- mu:    DoubleParam
-- sigma: DoubleParam
normal :: Likelihood
normal s d | (sigma <= 0) = zeroProb
           | otherwise    = normalDens x mu sigma
    where psts       = getDSOthers s d
          [mu,sigma] = map (doubleVal . flip getTagged psts) ["mu","sigma"]
          x          = doubleVal . head $ getDSData s d

-- mu:    DoubleParam
-- sigma: DoubleParam
lognormal :: Likelihood
lognormal s d | (sigma <= 0) = zeroProb
              | (x <= 0)     = zeroProb
              | otherwise    = lognormalDens x mu sigma
    where psts       = getDSOthers s d
          [mu,sigma] = map (doubleVal . flip getTagged psts) ["mu","sigma"]
          x          = doubleVal . head $ getDSData s d
                       
-- tau: DoubleParam                   
tempering :: Likelihood
tempering s d = logFloat $ (fromLogFloat $ density s cd) ** (1 / tau)
    where cd  = head . children $ d
          tau = doubleVal . getTagged "tau" $ getDSOthers s d

vaguePositive :: Likelihood
vaguePositive s d = logFloat $ 1 / x
    where x = doubleVal . head $ getDSData s d

dirac :: Likelihood
dirac s d | (x == fixed) = oneProb
          | otherwise    = zeroProb
    where fixed = datum . head $ getDSOthers s d
          x     = datum . head $ getDSData   s d

vague :: Likelihood
vague s d = oneProb

-- Methods for "manually" building densities

mkDensity :: [Density] -> [TA] -> [TA] -> Likelihood -> Density
mkDensity ds dsd dso l = mkNode (DNode l dsd dso) `collectDensities` ds

productDensity :: [Density] -> Density
productDensity ds = mkDensity ds [] [] multiplicative
                           
dummyDensity :: Density
dummyDensity = mkDensity [] [] [] vague

collectDensities :: Density -> [Density] -> Density
collectDensities dp ds = foldl (\p d -> addAt p [] d) dp ds

mkACDensity :: Density -> TA -> [TA] -> Density
mkACDensity cd h dso = mkNode (ACDNode h dso) `addChild` cd
                        
-- Run a Machine in a suitably generic manner

type Location = (TA,Tag)
type ReportAct = [Machine] -> IO [Machine]
    
showVals :: State -> [Location] -> String
showVals s ls = intercalate ", " (map sv ls)
    where sv (ts,t) = show . datum $ getTagAt t (tagsToAddress s ts) s

trace :: [Location] -> ReportAct
trace ls machines = do
  let tr m = printf "%s: %.3g" (showVals (ms m) ls)
             (fromLogFloat . topDensity $ m :: Double)       

  mapM_ (putStrLn . tr) machines
  return machines
         
stopAfter :: Int -> ReportAct
stopAfter i = return . take i
         
burnin :: Int -> ReportAct
burnin i = return . drop i

skim :: Int -> ReportAct
skim i = return . takeEvery i
    where takeEvery n = map snd .
                        filter ((== 0) . flip mod n . fst) .
                        zip [0..]

-- Currently only usable on double parameters
dump :: [Location] -> ReportAct
dump ls machines = (writeFile "dump.csv" $ unlines $ header : rows) >>
                   return machines
    where header  = intercalate "," $ "like" : (map snd ls)
          val s (ts,t) = show . doubleVal $ getTA (ts ++ [t]) s
          vals m       = map (val $ ms m) ls
          like m       = show $ (fromLogFloat . topDensity $ m :: Double)
          row m        = intercalate "," $ (like m) : (vals m)
          rows         = map row machines

run :: (Entropy -> Machine) -> IO [Machine]
run buildMachine = do
  e <- getRandoms

  let m = buildMachine e
  return $ iterate sampMach m
