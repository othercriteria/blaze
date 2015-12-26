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

data Env = EnvEmpty
         | Env { id::String, boundState::State, boundEnv::Env, restEnv::Env }

envEmpty :: Env -> Bool
envEmpty EnvEmpty = True
envEmpty _        = False
           
showEnv :: Env -> String
showEnv EnvEmpty = "(empty env)"
showEnv (Env i bs be re) | (envEmpty re) = se i bs be
                         | otherwise     = se i bs be ++ ", " ++ showEnv re
    where se i bs be = printf "%s => (%s, %s)" i (showE bs) (showEnv be)
    
showEE :: (State,Env) -> String
showEE (s,e) = printf "Exp: %s\nEnv: %s" (showE s) (showEnv e)

isValue :: State -> Bool
isValue (Node (SNode _ (DoubleDatum _     )) _  _) = True
isValue (Node (SNode _ (StringDatum "list")) cs _) = all isValue cs
isValue (Node (SNode _ (StringDatum ctag  )) cs _) =
    ctag `elem` ["thunk","lambda","true","false"]

notValue :: State -> Bool
notValue = not . isValue
         
notAllValue :: [State] -> Bool
notAllValue = not . all isValue

envjoin :: Env -> Env -> Env
envjoin e1               EnvEmpty             = e1
envjoin EnvEmpty         e2                   = e2
envjoin (Env i bs be re) (Env i' bs' be' re') =
    (Env i bs be (Env i' bs' be' (envjoin re re')))
                  
envlookup :: String -> Env -> Maybe (State,Env)
envlookup lid EnvEmpty = Nothing
envlookup lid (Env i bs be re)
    | (lid == i) = Just (bs,be)
    | otherwise  = envlookup lid re

evalhelper :: State -> String -> State
evalhelper sl btag = (Node (SNode [] newctag) newvals btag)
    where (Node (SNode [] (StringDatum "list")) (newhead:newvals) _) = sl
          (Node (SNode [] newctag             ) _                 _) = newhead

ifhelper :: State -> State -> State -> String -> State
ifhelper (Node (SNode [] (StringDatum "true" )) [] _) st sf btag = st `tag` btag
ifhelper (Node (SNode [] (StringDatum "false")) [] _) st sf btag = sf `tag` btag
ifhelper _                                            _  _  _    =
    error "ifhelper: conditional must be a boolean"

apphelper :: State -> State -> String -> Env -> (State,Env)
apphelper op operand btag env = (ope `tag` btag, newenv)
    where (Node (SNode [] (StringDatum "lambda")) [opv,ope] _) = op
          newenv = Env (stringVal opv) operand EnvEmpty env

varhelper :: State -> Env -> (State,Env)
varhelper v env = (s, (envjoin e env))
    where vl = envlookup (stringVal v) env
          s  = maybe v        fst vl
          e  = maybe EnvEmpty snd vl

allhelper :: [State] -> String -> State
allhelper cs btag = if alltrue then trueE else falseE
    where alltrue = all ((== "true") . stringVal) cs 

holehelper :: String -> [State] -> String -> Env -> (State,Env)
holehelper ct cs bt env = ((Node (SNode [] (StringDatum ct)) cs' bt), env')
    where (vs,(e:es)) = break (not . isValue) cs
          (e', env')  = step (e, env)
          cs'         = vs ++ [e'] ++ es

step :: (State,Env) -> (State,Env)
step ((Node (SNode [] (StringDatum ctag )) cs@[c]        btag), env)
    | (ctag == "eval"   ) = (evalhelper c btag, env)
    | (notAllValue cs   ) = holehelper ctag cs btag env
    | (ctag == "unthunk") = (head . children $ c, env)
    | (ctag == "abs"    ) = (numE (abs $ doubleVal c), env)
    | (ctag == "neg"    ) = (numE (negate $ doubleVal c), env)
    | (ctag == "flip"   ) = (if (doubleVal c > 0.5) then trueE else falseE, env)
    | (ctag == "sampint") = (numE (fromIntegral $ floor (doubleVal c / 2)), env)
    | (ctag == "draw"   ) = (head . children $ c, env)
    | (ctag == "and"    ) = (allhelper cs btag, env)
step ((Node (SNode [] (StringDatum ctag )) cs@[c1,c2]    btag), env)
    | (notAllValue cs) = holehelper ctag cs btag env
    | (ctag == "app" ) = apphelper c1 c2 btag env
    | (ctag == "+"   ) = (numE (doubleVal c1 + doubleVal c2), env)
    | (ctag == "-"   ) = (numE (doubleVal c1 - doubleVal c2), env)
    | (ctag == "*"   ) = (numE (doubleVal c1 * doubleVal c2), env)
    | (ctag == "exp" ) = (numE (doubleVal c1 ** doubleVal c2), env)
    | (ctag == "max" ) = (numE (max (doubleVal c1) (doubleVal c2)), env)
    | (ctag == "and" ) = (allhelper cs btag, env)
step ((Node (SNode [] (StringDatum "if" )) cs@[c1,c2,c3] btag), env)
    | (notValue c1) = holehelper "if" cs btag env
    | otherwise     = (ifhelper c1 c2 c3 btag, env)
step ((Node (SNode [] (StringDatum "let")) cs@[c1,c2,c3] btag), env) =
    (c3, Env (stringVal c1) c2 env env)
step ((Node (SNode [] (StringDatum ctag )) cs            btag), env)
    | (notAllValue cs) = holehelper ctag cs btag env
    | (ctag == "and" ) = (allhelper cs btag, env)
step (varstate                                                , env) =
    varhelper varstate env

evaluate :: State -> [(State,Env)]
evaluate s = sh ++ [head st]
    where ss      = iterate step (s, EnvEmpty)
          (sh,st) = break (isValue . fst) ss
         
-- Code except the program to be executed and the query.
sdefs :: State
sdefs =
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

sr :: State
sr = dummyState `collectStates` [scode,shistory]
    where scode    = sdefs `tag` "code"
          shistory = dummyState `tag` "history"

traceChurch :: TA -> ReportAct
traceChurch ta machines = do
  let tr m = printf "%s: %.3g" (showE . getTA ta $ ms m)
             (fromLogFloat . topDensity $ m :: Double)

  mapM_ (putStrLn . tr) machines
  return machines
                     
buildMachine :: Entropy -> Machine
buildMachine e = Machine sr dummyDensity mkIKernel e
                              
main :: IO ()
main = foldl (>>=) (run buildMachine) [ stopAfter 10, traceChurch ["code"] ] >>
       putStrLn "Run complete!"
