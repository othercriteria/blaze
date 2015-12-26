{-# LANGUAGE TypeSynonymInstances #-}

module Math.Statistics.Inference.Blaze
    where

-- Immutable tree data structure with fairly easy/cheap updates
import Data.BlazeTree
      
import Data.Number.LogFloat hiding (log)
import Data.List
import qualified Numeric.GSL.Special.Gamma as Gamma
import Text.Printf
import Control.Monad.Random

-- Renaming GSL imports
betaDensity = Gamma.beta
choose = Gamma.choose

-- Expose a function for tagging nodes
tag :: Tree a -> String -> Tree a
tag = tagNode

type TA = TagAddress
    
-- Transparently represent probabilities in log-space to
-- avoid underflows

type Prob = LogFloat

zeroProb :: Prob
zeroProb = logFloat (0.0 :: Double)

oneProb :: Prob
oneProb = logFloat (1.0 :: Double)

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

advance :: Machine -> Machine
advance m = sampleNextState (mk m) m

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
           | NullDatum
             deriving (Eq)

instance Show Datum where
    show (IntDatum    i) = printf "Int %d" i
    show (DoubleDatum d) = printf "Double %.3f" d
    show (NullDatum    ) = "Null"
                      
intVal :: State -> Int
intVal = iv . value . tvalue

doubleVal :: State -> Double
doubleVal = dv . value . tvalue

setVal :: State -> Datum -> State
setVal s dat = s { tvalue = (tvalue s) { value = dat } }

depEdges :: State -> [TA]
depEdges = ssDEdges . tvalue

-- Structure to represent Density tree

type Density = Tree DNode

data DNode = DNode   { likelihood::Likelihood,  dsData::[TA],  dsOthers::[TA] }
           | ACDNode { acdHead::TA,            acdData::[TA], acdOthers::[TA] }
               
instance Show DNode where
    show (DNode   l d o) = printf "(Density d: %s o: %s)" (show d) (show o) 
    show (ACDNode h d o) =
        printf "(CDensity for %s, d: %s, o: %s)" (showTags h) (show d) (show o)
                                    
instance Eq DNode where
    (DNode   _  d1 o1) == (DNode   _  d2 o2) = d1 == d2 && o1 == o2
    (ACDNode h1 d1 o1) == (ACDNode h2 d2 o2) = h1 == h2 && d1 == d2 && o1 == o2
    _                  == _                  = False

type Likelihood = State -> Density -> Prob

addDSData :: Density -> [TA] -> Density
addDSData d tas = d { tvalue = dv { dsData = tas ++ (dsData dv) } }
    where dv = tvalue d

addDSOthers :: Density -> [TA] -> Density
addDSOthers d tas = d { tvalue = dv { dsOthers = tas ++ (dsOthers dv) } }
    where dv = tvalue d
               
density :: Likelihood
density s d@(Node (DNode l _ _)  _    _) = l s d
density s   (Node (ACDNode h ds os) [cd] _) = density s d'
    where d' = mkACDensity [cd,dummyDensity] h ds os
density s   (Node (ACDNode h ds os) cds  _) = product $ map asc shcts
    where asc t  = density s (cdt' t) * density (s' t) cd

          cdt    = getTagged "top" cds
          cd     = getTagged ""    cds

          sha    = tagsToAddress s h
          sh     = getAt sha s
          shcts  = map tagged $ children sh

          cdt' t = flip addDSData   (map (\d -> h ++ [t] ++ d) ds) $
                   flip addDSOthers (map (\o -> h ++ [t] ++ o) os) $
                   cdt     
          s'   t = getAt (tagsToAddress s (h ++ [t])) s
                
topDensity :: Machine -> Prob
topDensity m = density (ms m) (md m)

-- Structure to represent Kernel tree

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

type Kernel = Tree KNode

data KNode = GP Double TA
           | DP [Datum] TA
           | RDSL TA
           | RDDL TA Tag Tag
           | CC
           | CM [Double]
           | CH TA (State -> Tag)
           | I
           | MH
           | EG
           | VC TA (TA -> Kernel)
           | VM TA (TA -> Kernel)

instance Eq KNode where
    (GP s1 ts1) == (GP s2 ts2) = (s1 == s2 && ts1 == ts2)
    (DP ds1 ts1) == (DP ds2 ts2) = (ds1 == ds2 && ts1 == ts2)
    (RDSL ts1) == (RDSL ts2) = (ts1 == ts2)
    (RDDL ts1 t1 t1') == (RDDL ts2 t2 t2') =
        (ts1 == ts2 && t1 == t2 && t1' == t2')
    CC == CC = True
    (CM ws1) == (CM ws2) = (ws1 == ws2)
    (CH ts1 _) == (CH ts2 _) = (ts1 == ts2)
    I == I = True
    MH == MH = True
    EG == EG = True
    (VC ts1 tsk1) == (VC ts2 tsk2) = (ts1 == ts2)
    (VM ts1 tsk1) == (VM ts2 tsk2) = (ts1 == ts2)
                      
instance Show KNode where
    show (GP scale tags) =
        printf "GP Kernel: scale = %f at %s" scale (showTags tags)
    show (DP ds tags) =
        printf "DP Kernel: vals. = %s at %s" (show ds) (showTags tags)
    show (RDSL tags) = printf "RDSL Kernel: coll. %s" (showTags tags)
    show (RDDL tags tag1 tag2) =
        printf "RDDL Kernel: coll. %s: {%s} -> {%s}" (showTags tags) tag1 tag2
    show CC = "CC Kernel"
    show (CM ws) =
        printf "CM Kernel: weights [%s]" (intercalate "," $ map show ws)
    show (CH tags _) = printf "CH Kernel on state %s" (showTags tags)
    show I = "No action"
    show MH = "Metropolis-Hastings adaptor"
    show EG = "Enumerative Gibbs sampling adaptor"
    show (VC tags _) = printf "VC Kernel: coll. %s" (showTags tags)
    show (VM tags _) = printf "VM Kernel: coll. %s" (showTags tags)

showTags :: TA -> String
showTags ts = "[{" ++ intercalate "},{" (map show ts) ++ "}]"

class Sample a where
    sampleNextState :: a -> Machine -> Machine
    sampleNextMove :: a -> Machine -> Move
    enumeratePossibleMoves :: a -> Machine -> [Move]
                              
data Move = Move { target::Machine
                 , forwardDens::Prob
                 , reverseDens::Prob
                 , jacobian::Double
                 }

-- TODO: moves should use some monadic structure
          
-- This only makes sense when m' has been generated starting with m
joinTo :: Move -> Move -> Move
joinTo m m' = Move t f r j
    where t = target m'
          f = forwardDens m * forwardDens m'
          r = reverseDens m * reverseDens m'
          j = jacobian m * jacobian m'

-- Implementation of Kernels

-- Pick functions consume entropy to choose between options

pick :: Double -> [a] -> a
pick e xs = pickw e (repeat (1 / (fromIntegral $ length xs))) xs

pickw :: Double -> [Double] -> [a] -> a
pickw _ _      []     = error "pickw: nothing to choose from"
pickw e (w:ws) (x:xs) | (e > 0 && (e - w) < 0) = x
                      | otherwise              = pickw (e - w) ws xs

toWeights :: [Int] -> [Double]
toWeights is = map (/ (sum is')) is'
    where is' = map fromIntegral is
                                                 
-- Recall from Bonawitz pg. 51 that Kernels are not intended to modify
-- the Density graph in any way.

instance Sample Kernel where
    sampleNextState (Node (RDDL tags tag1 tag2) _ _) m@(Machine s _ _ (e:es)) =
        m { ms = news', me = es }
        where cra    = tagsToAddress s tags
              cr     = getAt cra s

              srccc  = untaggedChild . getTagged tag1 $ children cr
              srcx   = pick e $ children srccc
                     
              tarc   = getTagged tag2 $ children cr
              tarcca = cra ++ address cr [ (== tarc), not . isTagged ]

              news   = addAt s tarcca srcx

              cra'   = tagsToAddress news tags
              cr'    = getAt cra' news
              srcc'  = getTagged tag1 $ children cr'
              srcxa' = cra' ++
                       address cr' [ (== srcc'), not . isTagged, (== srcx) ]
              
              news'  = removeAt news srcxa'

    sampleNextState (Node (CM ws) cs _) m@(Machine _ _ _ (e:es)) =
        sampleNextState (pickw e ws cs) (m { me = es })
              
    sampleNextState (Node CC cs _) m = foldr sampleNextState m cs
              
    sampleNextState (Node (CH ts pred) cs _) m@(Machine s _ _ _) =
        sampleNextState c m
            where ct = pred (getAt (tagsToAddress s ts) s)
                  c  = getTagged ct cs

    sampleNextState (Node I _ _) m = m

    sampleNextState (Node MH [c] _) m@(Machine _ _ _ (e:es)) =
        if (e < fromLogFloat pAcc)
        then target move
        else m'
            where m'   = m { me = es }
                  move = sampleNextMove c m'

                  dPre = topDensity m'
                  dPst = topDensity . target $ move
                  pFrw = forwardDens move
                  pRev = reverseDens move

                  pAcc = min 1 ((dPst / dPre) * (pRev / pFrw) *
                                (logFloat $ jacobian move))

    sampleNextState (Node (VM tags tsk) _ _) m@(Machine s _ _ (e:es)) =
        sampleNextState (tsk $ tags ++ [crct]) m'
            where m' = m { me = es }

                  cra  = tagsToAddress s tags
                  crcs = children $ getAt cra s
                  crct = tagged $ pick e crcs

    sampleNextState k m = target $ sampleNextMove k m
                            
    sampleNextMove (Node (GP scale ts) _ _) m@(Machine s _ _ (e1:e2:es)) =
        Move (m { ms = news, me = es }) dens dens 1.0
            where oldsa = tagsToAddress s ts
                  oldpa = init oldsa
                  oldsd = getAt oldsa s
                  norm  = sqrt ((-2) * log e1) * cos (2 * pi * e2)
                  dens  = normalDens norm 0.0 1.0
                  newv  = doubleVal oldsd + scale * norm
                  newsd = mkDoubleParam (last ts) newv
                  news  = update s oldpa oldsd newsd

    sampleNextMove (Node (DP datums ts) _ _) m@(Machine s _ _ (e:es)) =
        Move (m { ms = news, me = es }) dens dens 1.0
            where oldsa = tagsToAddress s ts
                  oldpa = init oldsa
                  oldsd = getAt oldsa s
                  dens  = logFloat $
                          (1 / (fromIntegral . length $ datums) :: Double)
                  newsd = setVal oldsd (pick e datums)
                  news  = update s oldpa oldsd newsd
                          
    sampleNextMove k@(Node (RDSL tags) cs tag) m@(Machine s _ _ (e1:e2:es)) =
        Move tar dens dens 1.0
            where tar = sampleNextState (Node (RDDL tags srct tart) cs tag) m'

                  coll    = getAt (tagsToAddress s tags) s
                  (ts,ns) = unzip . summarizeComps $ coll
                           
                  srct   = pickw e1 (toWeights ns) ts
                  tart   = pick  e2 ts
                  m'     = m { me = es }

                  dens = logFloat $
                         (1 / fromIntegral (length ns * sum ns) :: Double)

    sampleNextMove (Node (CM ws) cs _) m@(Machine _ _ _ (e:es)) =
        Move tar (logFloat w * fdens) (logFloat w * rdens) jac
            where m'     = m { me = es }
                  (c,w)  = pickw e ws (zip cs ws)

                  move  = sampleNextMove c m'
                  tar   = target move
                  fdens = forwardDens move
                  rdens = reverseDens move
                  jac   = jacobian move
                  
    sampleNextMove (Node CC [] _) m = sampleNextMove mkIKernel m

    sampleNextMove (Node CC (c:cs) _) m = this `joinTo` rest
            where this = sampleNextMove c m
                  rest = sampleNextMove (mkCCKernel cs) (target this)
                         
    sampleNextMove (Node I _ _) m = Move m oneProb oneProb 1.0

    sampleNextMove (Node (VC tags tsk) _ _) m@(Machine s _ _ _) =
        sampleNextMove (mkCCKernel $ map tsk crcas) m
            where cra   = tagsToAddress s tags
                  crcts = map tagged $ children $ getAt cra s
                  crcas = map (\t -> tags ++ [t]) crcts
             
    sampleNextMove (Node (VM tags tsk) _ _) m@(Machine s _ _ (e:es)) =
        Move tar (cdens * fdens) (cdens * rdens) jac
            where m' = m { me = es }

                  cra   = tagsToAddress s tags
                  crcs  = children $ getAt cra s
                  crct  = tagged $ pick e crcs
                  cdens = logFloat $ (1 / fromIntegral (length crcs) :: Double)

                  move  = sampleNextMove (tsk $ tags ++ [crct]) m'
                  tar   = target move
                  fdens = forwardDens move
                  rdens = reverseDens move
                  jac   = jacobian move

    sampleNextMove k m@(Machine _ _ _ (e:es)) =
        pickw e ws moves
            where moves = enumeratePossibleMoves k (m { me = es })
                  ws    = map (fromLogFloat . forwardDens) moves

    enumeratePossibleMoves (Node (DP datums ts) _ _) m@(Machine s _ _ _) =
        [ Move (m { ms = news }) dens dens 1.0 | news <- newss ]
            where oldsa  = tagsToAddress s ts
                  oldpa  = init oldsa
                  oldsd  = getAt oldsa s
                  dens   = logFloat $
                           (1 / (fromIntegral . length $ datums) :: Double)
                  newsds = map (setVal oldsd) datums
                  newss  = map (update s oldpa oldsd) newsds
                          
    enumeratePossibleMoves k@(Node (RDSL tags) _ _) m@(Machine s _ _ _) =
        [ Move (m { ms = news' }) dens dens 1.0 | news' <- newss' ]
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

                  dens = logFloat $
                         (1 / fromIntegral (length newss) :: Double)            

    enumeratePossibleMoves (Node CC [] _) m = [ sampleNextMove mkIKernel m ]

    enumeratePossibleMoves (Node CC (c:cs) _) m = concatMap restFrom these
            where these      = enumeratePossibleMoves c m
                  rest t     = enumeratePossibleMoves (mkCCKernel cs) (target t)
                  restFrom t = [ t `joinTo` r | r <- rest t ]
                         
    enumeratePossibleMoves (Node EG [c] _) m =
        zipWith4 mkMove tars denss' hdensss rdensss
            where moves = enumeratePossibleMoves c m
                  tars  = map target moves
                          
                  denss  = map topDensity tars
                  denss' = map (/ (sum denss)) denss

                  rmovess = map (enumeratePossibleMoves (mkEGKernel c)) tars
                  rtarss  = map (map target) rmovess
                  rdensss = map (map topDensity) rtarss
                  hitms   = zipWith (\a b -> filter ((== (ms a)) . ms) b)
                            tars rtarss
                  hdensss = map (map topDensity) hitms

                  mkMove t d hd rd = Move t d (sum hd / sum rd) 1.0
      
    enumeratePossibleMoves k _ = error $ printf "Cannot enumerate %s" (show k)

collectKernels :: Kernel -> [Kernel] -> Kernel
collectKernels k ks = foldl (\c k' -> addAt c [] k') k ks
                             
mkGPKernel :: Double -> TA -> Kernel
mkGPKernel scale tags = mkNode $ GP scale tags

mkDPKernel :: [Datum] -> TA -> Kernel
mkDPKernel datums tags = mkNode $ DP datums tags
                            
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

mkVCKernel :: TA -> (TA -> Kernel) -> Kernel
mkVCKernel tags tsk = mkNode (VC tags tsk)
               
mkVMKernel :: TA -> (TA -> Kernel) -> Kernel
mkVMKernel tags tsk = mkNode (VM tags tsk)
                           
-- Structure to represent entropy source: these should be uniformly
-- distributed draws from the interval [0,1].

type Entropy = [Double]
                             
-- Methods for building States

mkDoubleSNode :: Double -> SNode
mkDoubleSNode d = SNode [] (DoubleDatum d)

mkDoubleData :: Double -> State
mkDoubleData = mkNode . mkDoubleSNode

mkDoubleParam :: Tag -> Double -> State
mkDoubleParam t d = mkDoubleData d `tagNode` t

mkDoubleParams :: TA -> [Double] -> [State]
mkDoubleParams = zipWith mkDoubleParam
                    
mkIntSNode :: Int -> SNode
mkIntSNode i = SNode [] (IntDatum i)

mkIntData :: Int -> State
mkIntData = mkNode . mkIntSNode

mkIntParam :: Tag -> Int -> State
mkIntParam t i = mkIntData i `tagNode` t

mkIntParams :: TA -> [Int] -> [State]
mkIntParams = zipWith mkIntParam

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
getDSData s d = map (flip getAt s . tagsToAddress s) (dsData . tvalue $ d)

getDSOthers :: State -> Density -> [State]
getDSOthers s d = map (flip getAt s . tagsToAddress s) (dsOthers . tvalue $ d)

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
                           exp (-(log x - (mu**2)) / (2 * sigma**2))
                            
-- j:    DoubleParam (interaction strength)
-- h:    DoubleParam (potential strength)
-- temp: DoubleParam (temperature)
ising :: Likelihood
ising s d | (t <= 0) = zeroProb
          | otherwise   = logFloat $ max 0 ((p x) / (p x + p (-x)))
          where psts    = getDSOthers s d
                [j,h,t] = map (doubleVal . flip getTagged psts) ["j","h","temp"]

                xst     = head $ getDSData s d
                x       = fromIntegral . intVal $ xst
                nstas   = depEdges xst
                nsas    = map (tagsToAddress s) nstas
                ns      = map (flip getAt s) nsas
                nsum    = fromIntegral . sum . map intVal $ ns

                e x'    = ((-1)/2) * j * x' * nsum - h * x
                p x'    = (exp (-(e x') / t))
         
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

{-
-- Hopefully, there's some more generic way to accomplish the Dirac
-- delta Densities.

-- fixedi: IntParam
diracInt :: Likelihood
diracInt s d | (f == x)  = oneProb
             | otherwise = zeroProb
    where (fst,[xst]) = splitTagged . dsEdges . tvalue $ d
          f           = intVal $ getTagged "fixedi" fst
          x           = intVal xst

-- fixedd: DoubleParam
dirac :: Likelihood
dirac d | (f == x)  = oneProb
        | otherwise = zeroProb
    where (fst,[xst]) = splitTagged . dsEdges . tvalue $ d
          f           = doubleVal $ getTagged "fixedd" fst
          x           = doubleVal xst
                                     
-- p: DoubleParam
binomial :: Likelihood
binomial d = logFloat $ norm * like
    where ([pst],xsts) = splitTagged . dsEdges . tvalue $ d
          p            = doubleVal pst
          xs           = map intVal xsts

          n = length xs
          x = sum xs

          norm = (fromIntegral n) `choose` (fromIntegral x)
          like = (p ** fromIntegral x) * ((1 - p) ** (fromIntegral $ n - x))

-}

vague :: Likelihood
vague s d = oneProb

-- Methods for building Densities

-- Methods for "manually" building densities

mkDensity :: [Density] -> [TA] -> [TA] -> Likelihood -> Density
mkDensity ds dsd dso l = mkNode (DNode l dsd dso) `collectDensities` ds

productDensity :: [Density] -> Density
productDensity ds = mkDensity ds [] [] multiplicative
                           
dummyDensity :: Density
dummyDensity = mkDensity [] [] [] vague

collectDensities :: Density -> [Density] -> Density
collectDensities c ds = foldl (\c d -> addAt c [] d) c ds

mkACDensity :: [Density] -> TA -> [TA] -> [TA] -> Density
mkACDensity cds h dsd dso = mkNode (ACDNode h dsd dso) `collectDensities` cds
                        
-- Run a machine in a suitably generic manner
type Location = (TA,Tag)
type ReportAct = [Machine] -> IO ()
    
showVals :: State -> [Location] -> String
showVals s ls = intercalate ", " (map sv ls)
    where sv (ts,t) = show . value . tvalue $ getTagAt t (tagsToAddress s ts) s

trace :: [Location] -> ReportAct
trace ls = mapM_ (putStrLn . tr)
    where tr m = printf "%s: %s" (showVals (ms m) ls) (show . topDensity $ m)

burnin :: Int -> ReportAct -> ReportAct
burnin i r = r . drop i

skim :: Int -> ReportAct -> ReportAct
skim i r = r . takeEvery i
    where takeEvery n = map snd .
                        filter ((== 0) . flip mod n . fst) .
                        zip [0..]

-- Currently only usable on double parameters
dump :: [Location] -> ReportAct
dump ls machines = writeFile "dump.csv" $ unlines $ header : rows
    where header  = intercalate "," $ "like" : (map snd ls)
          val s (ts,t) = show . doubleVal $ getTagAt t (tagsToAddress s ts) s
          vals m       = map (val $ ms m) ls
          like m       = show $ (fromLogFloat . topDensity $ m :: Double)
          row m        = intercalate "," $ (like m) : (vals m)
          rows         = map row machines
                 
run :: (Entropy -> Machine) -> Int -> [ReportAct] -> IO ()
run buildMachine iters actions = do
  e <- getRandoms

  let m   = buildMachine e
  let sim = take iters $ iterate advance m

  mapM_ ($ sim) actions