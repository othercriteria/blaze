module RAE
    where

import Blaze
      
import Tree
import Text.Printf
import Data.Number.LogFloat (logFloat,fromLogFloat)
import Data.List

-- Pretty-print expressions
showE :: State -> String
showE (Node (SNode _ (DoubleDatum d)) []         _) = printf "%0.3f" d
showE (Node (SNode _ (StringDatum s)) []         _) = s
showE (Node (SNode _ (StringDatum s)) cs         _)
    | (s == "and" ) = printf "(and %s)" (intercalate " " scs)
    | (s == "list") = printf "(list %s)" (intercalate " " scs)
    where scs = map showE cs
showE (Node (SNode _ (StringDatum s)) [c1]       _)
    | (s == "eval"   ) = printf "(eval %s)" sc1
    | (s == "thunk"  ) = printf "(thunk %s)" sc1
    | (s == "unthunk") = printf "(unthunk %s)" sc1
    | (s == "abs"    ) = printf "abs(%s)" sc1
    | (s == "neg"    ) = printf "-(%s)" sc1
    | (s == "flip"   ) = printf "flip(%s)" sc1
    | (s == "sampint") = printf "sampint(%s)" sc1
    | (s == "draw"   ) = printf "draw(%s)" sc1
    | (s == "num"    ) = sc1
    | (s == "var"    ) = sc1
    | otherwise        = error $ printf "showE: invalid unary op %s" s
    where sc1 = showE c1
showE (Node (SNode _ (StringDatum s)) [c1,c2]    _)
    | (s == "lambda") = printf "(lambda %s %s)" sc1 sc2
    | (s == "app"   ) = printf "(%s %s)" sc1 sc2
    | (s == "+"     ) = printf "(%s + %s)" sc1 sc2
    | (s == "-"     ) = printf "(%s - %s)" sc1 sc2
    | (s == "*"     ) = printf "(%s * %s)" sc1 sc2
    | (s == "exp"   ) = printf "(%s ^ %s)" sc1 sc2
    | (s == "max"   ) = printf "max(%s, %s)" sc1 sc2
    | otherwise       = error $ printf "showE: invalid binary op %s" s
    where [sc1,sc2] = map showE [c1,c2]
showE (Node (SNode _ (StringDatum s)) [c1,c2,c3] _)
    | (s == "let" ) = printf "(let (%s = %s) in %s)" sc1 sc2 sc3
    | (s == "if"  ) = printf "(if %s then %s else %s)" sc1 sc2 sc3
    | otherwise     = error $ printf "showE: invalid ternary op %s" s
    where [sc1,sc2,sc3] = map showE [c1,c2,c3]
showE (Node (SNode _ (StringDatum s)) cs         _)
    | otherwise     = error $ printf "showE: invalid multi op %s" s
    where scs = map showE cs
       
-- Helper functions for building expressions.
nullaryOp :: String -> State
nullaryOp s = mkStringData s

unaryOp :: String -> State -> State
unaryOp s e1 = mkStringData s `addChild` e1

binaryOp :: String -> State -> State -> State
binaryOp s e1 e2 = mkStringData s `collectStates` [e2,e1]

ternaryOp :: String -> State -> State -> State -> State
ternaryOp s e1 e2 e3 = mkStringData s `collectStates` [e3,e2,e1]

multiOp :: String -> [State] -> State
multiOp s es = mkStringData s `collectStates` (reverse es)

-- In the absence of a parser, I use these functions to build an AST
thunkE   = unaryOp   "thunk"
unthunkE = unaryOp   "unthunk"
lambdaE  = binaryOp  "lambda"
appE     = binaryOp  "app"               
evalE    = unaryOp   "eval"
letE     = ternaryOp "let"
absE     = unaryOp   "abs"
negE     = unaryOp   "neg"
addE     = binaryOp  "+"
subE     = binaryOp  "-"
mulE     = binaryOp  "*"
expE     = binaryOp  "exp"
maxE     = binaryOp  "max"
trueE    = nullaryOp "true"
falseE   = nullaryOp "false"
andE     = multiOp   "and"
ifE      = ternaryOp "if"
flipE    = unaryOp   "flip"
sampintE = unaryOp   "sampint"
drawE    = unaryOp   "draw"
listE    = multiOp   "list"
numE     = mkDoubleData
varE     = mkStringData

-- Building an evaluator for testing; this will eventaully be worked
-- into a Kernel and Density.

getCode :: State -> State
getCode = getTA ["code"]

getSteps :: State -> State
getSteps = getTA ["steps"]

getProbs :: State -> State
getProbs = getTA ["probs"]

getEnv :: State -> State
getEnv = getTA ["env"]
         
showEnv :: State -> String
showEnv s | (null senvcs) = "(empty env)"
          | otherwise     = intercalate ", " $ map se senvcs
          where senvcs = children $ getEnv s
                se e   = printf "%s => (%s, %s)" (tagged e)
                       (showE $ getTA ["bstate"] s) (showEnv $ getTA ["benv"] s)
    
showNode :: State -> String
showNode s = printf "Code: %s\nEnv: %s\nProb: %s" (showE . getCode $ s)
             (showEnv s)
             (show . map doubleVal . children . getProbs $ s)

isValue :: State -> Bool
isValue (Node (SNode _ (DoubleDatum _     )) _  _) = True
isValue (Node (SNode _ (StringDatum "list")) cs _) = all isValue cs
isValue (Node (SNode _ (StringDatum ctag  )) cs _) =
    ctag `elem` ["thunk","lambda","true","false"]

envlookup :: String -> State -> Maybe State
envlookup ltag s | (ltag `elem` map tagged cs) = Just (getTagged ltag cs)
                 | otherwise                   = Nothing
                 where cs = children . getEnv $ s

evalhelper :: State -> ([State],[Double],[State])
evalhelper sl = ([(Node (SNode [] newctag) newvals "")], [1.0], [])
    where (Node (SNode [] (StringDatum "list")) (newhead:newvals) _) = sl
          (Node (SNode [] newctag             ) _                 _) = newhead

ifhelper :: State -> State -> State -> ([State],[Double],[State])
ifhelper (Node (SNode [] (StringDatum "true" )) [] _) st sf = ([st], [1.0], [])
ifhelper (Node (SNode [] (StringDatum "false")) [] _) st sf = ([sf], [1.0], [])
ifhelper _                                            _  _  =
    error "ifhelper: conditional must be a boolean"

apphelper :: State -> State -> ([State],[Double],[State])
apphelper op operand = ([ope], [1.0], [operand `tag` (stringVal opv)])
    where (Node (SNode [] (StringDatum "lambda")) [opv,ope] _) = op

varhelper :: State -> State -> ([State],[Double],[State])
varhelper s v = ([s'], [1.0], [])
    where s' = maybe v id $ envlookup (stringVal v) s

andhelper :: [State] -> ([State],[Double],[State])
andhelper cs
    | (not allbool) = error "andhelper: expects only boolean values"
    | otherwise       = (if alltrue then [trueE] else [falseE], [1.0], [])
    where allbool = all (flip elem ["true","false"] . stringVal) cs
          alltrue = all ((== "true") . stringVal) cs

fliphelper :: State -> ([State],[Double],[State])
fliphelper c = ([trueE, falseE], [doubleVal c, 1.0 - doubleVal c], [])

sampinthelper :: State -> ([State],[Double],[State])
sampinthelper c = (vals, probs, [])
    where vals  = map numE [0..(doubleVal c)]
          nvals = length vals
          probs = replicate nvals (1 / fromIntegral nvals)

drawhelper :: State -> ([State],[Double],[State])
drawhelper c = (vals, probs, [])
    where vals  = children c
          nvals = length vals
          probs = replicate nvals (1 / fromIntegral nvals)
                  
holehelper :: State -> String -> [State] -> ([State],[Double],[State])
holehelper s ct cs = (map mkStep e', p', env')
    where (vs,(e:es))  = break (not . isValue) cs
          (e',p',env') = step' s e
          cs' x        = vs ++ [x] ++ es
          mkStep x     = (Node (SNode [] (StringDatum ct)) (cs' x) "")

notFinal :: State -> State -> Bool
notFinal s v@(Node (SNode [] (StringDatum _     )) [] _) = v /= v'
    where ([v'],_,_) = varhelper s v
notFinal s v@(Node (SNode [] (StringDatum "list")) cs _) = any (notFinal s) cs
notFinal _ v                                             = not . isValue $ v
                         
-- Step takes a state and produces a list of possible single-step
-- reductions, a list of associated probabilities, and a set of new
-- variable bindings to add to the environment.
step :: State -> ([State],[Double],[State])
step s = step' s (getCode s)

step' :: State -> State -> ([State],[Double],[State])
step' s    (Node (SNode [] (StringDatum ctag )) cs@[c]        _)
    | (ctag == "eval"   ) = evalhelper c
    | (notFinal s c     ) = holehelper s ctag cs
    | (ctag == "sampint") = sampinthelper c
    | (ctag == "unthunk") = ([head . children $ c], [1.0], [])
    | (ctag == "flip"   ) = fliphelper c
    | (ctag == "draw"   ) = drawhelper c
    | (ctag == "and"    ) = andhelper cs
    | (ctag == "abs"    ) = ([numE (abs $ doubleVal c)], [1.0], [])
    | (ctag == "neg"    ) = ([numE (negate $ doubleVal c)], [1.0], [])
step' s    (Node (SNode [] (StringDatum ctag )) cs@[c1,c2]    _)
    | (notFinal s c1) = holehelper s ctag cs
    | (ctag == "app" ) = apphelper c1 c2
    | (notFinal s c2 ) = holehelper s ctag cs
    | (ctag == "+"   ) = ([numE (doubleVal c1 + doubleVal c2)], [1.0], [])
    | (ctag == "-"   ) = ([numE (doubleVal c1 - doubleVal c2)], [1.0], [])
    | (ctag == "*"   ) = ([numE (doubleVal c1 * doubleVal c2)], [1.0], [])
    | (ctag == "exp" ) = ([numE (doubleVal c1 ** doubleVal c2)], [1.0], [])
    | (ctag == "max" ) = ([numE (max (doubleVal c1) (doubleVal c2))], [1.0], [])
    | (ctag == "and" ) = andhelper cs
step' s    (Node (SNode [] (StringDatum "let")) cs@[c1,c2,c3] _)
    | (notFinal s c2) = holehelper s "let" cs
    | otherwise       = ([c3], [1.0], [c2 `tag` (stringVal c1)])
step' s    (Node (SNode [] (StringDatum "if" )) cs@[c1,c2,c3] _)
    | (notFinal s c1) = holehelper s "if" cs
    | otherwise       = ifhelper c1 c2 c3
step' s s'@(Node (SNode [] (StringDatum ctag )) cs            _)
    | (any (notFinal s) cs) = holehelper s ctag cs
    | (ctag == "and"      ) = andhelper cs
    | (null cs            ) = varhelper s s'

-- Code except the program to be executed and the query.
sdefs :: State
sdefs = (andE [(flipE (numE 0.5)), (flipE (numE 0.2)), (flipE (numE 0.1))])
{-
    (letE (varE "noisy=")
     (lambdaE (varE "x")
      (lambdaE (varE "y")
       (flipE (expE (numE 0.1)
               (absE (subE (varE "x") (varE "y")))))))
     (letE (varE "rae")
      (thunkE sprogram)
      (letE (varE "proc-from-expr")
       (lambdaE (varE "expr")
        (evalE (listE [(varE "lambda"), (varE "x"), (varE "expr")])))
       (letE (varE "my-expr")
        (unthunkE (varE "rae"))
        (letE (varE "my-proc")
         (appE (varE "proc-from-expr") (varE "my-expr"))
         squery)))))
-}
-- Program that generates random arithmetic expressions.                
sprogram :: State
sprogram =
    (ifE (flipE (numE 0.8))
     (ifE (flipE (numE 0.5)) (varE "x") (sampintE (numE 10)))
     (listE [(drawE
              (listE [(varE "+"), (varE "-"), (varE "*"), (varE "max")])),
             (unthunkE (varE "rae")),
             (unthunkE (varE "rae"))]))

-- Query conditioned on the random arithmetic expression.    
squery :: State
squery =
    (andE [(appE (appE (varE "noisy=")
                  (appE (varE "my-proc") (numE (-2)))) (numE 5)),
           (appE (appE (varE "noisy=")
                  (appE (varE "my-proc") (numE 0))) (numE 1)),
           (appE (appE (varE "noisy=")
                  (appE (varE "my-proc") (numE 1))) (numE 2)),
           (appE (appE (varE "noisy=")
                  (appE (varE "my-proc") (numE 2))) (numE 5)),
           (appE (appE (varE "noisy=")
                  (appE (varE "my-proc") (numE 3))) (numE 10))])

buildTree :: State -> State
buildTree s
    | (isValue code) = build [code,dummyState,dummyState,env]
    | otherwise      = build [code,steps,     probs,     env]
    where code = getCode s
          env  = getEnv s

          (steps', probs', env') = step s
                     
          env''    = dummyState { children = (env' ++ children env) }
          mkHead x = dummyState `collectStates`
                     [x `tag` "code", env'' `tag` "env"]
          steps''  = map mkHead steps'
          
          steps = dummyState `collectStates` map buildTree steps''
          probs = dummyState `collectStates` map mkDoubleData probs'
          
          build cs = dummyState `collectStates`
                     zipWith tag cs ["code","steps","probs","env"]

sr :: State
sr = buildTree (dummyState `collectStates` [tcode,tenv])
     `collectStates` [tterm,trace]
    where tcode = sdefs `tag` "code"
          tenv  = dummyState `tag` "env"
          tterm = flip tag "term" $
                  dummyState `collectStates` [trueE `tag` "t", falseE `tag` "f"]
          trace = flip tag "trace" $
                  buildTree $
                  dummyState `collectStates`
                  [falseE `tag` "code", dummyState `tag` "env"]

sd :: Likelihood
sd s _ = logFloat $ doubleVal $ getTA ["prob"] s

kr :: Kernel
kr = mkURWKernel [["term","t"],["term","f"]]
                  
traceChurch :: TA -> ReportAct
traceChurch ta machines = do
  let tr m = printf "%s: %.3g" (showE . getTA ta $ ms m)
             (fromLogFloat . topDensity $ m :: Double)

  mapM_ (putStrLn . tr) machines
  return machines

traceChurchEnv :: TA -> String -> ReportAct
traceChurchEnv ta v machines = do
  let tr m = printf "%s: %.3g" (maybe "" showE . envlookup v . getTA ta $ ms m)
             (fromLogFloat . topDensity $ m :: Double)

  mapM_ (putStrLn . tr) machines
  return machines
         
buildMachine :: Entropy -> Machine
buildMachine e = Machine sr dummyDensity kr e
                              
main :: IO ()
main = foldl (>>=) (run buildMachine)
       [ stopAfter 100, traceChurch ["trace","code"]] >>
       putStrLn "Run complete!"
