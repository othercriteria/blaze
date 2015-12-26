module Bonawitz_3_16a
    where

import Blaze
import Tree

-- Actual data with means generated from N(0.5,1.2)
-- > c(rnorm(5,-0.36),rnorm(3,1.52),rnorm(7,1.13))
-- [1]  0.47292825  0.75540756 -1.88129332 -2.27875185 -0.42945479
-- [6]  1.27713236  1.52884804  1.11515755  0.85932051  0.68070500
-- [11]  2.10720307 -0.03267406  1.95447534  2.21044549  1.63612941

-- Root state
sr :: State
sr = collectStates dummyState $ (scs `tagNode` "scs") : sgps

-- Global parameters for group means
sgps :: [State]
sgps = mkDoubleParams ["mu","sigma"] [1.0, 1.0]

-- Collection state to hold components
scs :: State
scs = collectStates dummyCState [c1,c2,c3]
    where c1 = mkComp 1 [0.47,1.28,2.11,0.75,1.53]
          c2 = mkComp 2 [(-0.03),(-1.88),1.11,1.95,(-2.28)]
          c3 = mkComp 3 [0.85,2.21,(-0.43),0.68,1.63]

-- Each component has its own mu (which is adjusted by the Kernel) and
-- sigma (which is held fixed)
mkComp :: Int -> [Double] -> State
mkComp i xs = collectStates dummyState [mu,sigma,csx] `tagNode` ct
    where ct         = "c" ++ show i
          csx        = collectStates dummyCState $
                       zipWith (flip tagNode) (((ct ++) . show) `fmap` [1..]) $
                       map mkDoubleData xs
          [mu,sigma] = mkDoubleParams ["mu","sigma"] [0.0,1.0]

-- Likelihood depends on the likelihood of the component parameters
-- and the likelihood of the data in each component given its parameters.
dr :: Density
dr = mkACDensity (productDensity [dparam,dcomp]) ["scs"] [["mu"],["sigma"]]
     where dparam = mkDensity [] [["mu"]] [["dso","mu"],["dso","sigma"]] normal
           dcomp  = mkACDensity ddatum [""] [["mu"],["sigma"]]
           ddatum = mkDensity [] [[]] [["dso","mu"],["dso","sigma"]] normal
            
-- Kernel does uniform reassignment, perturbs the global parameters,
-- and perturbs each component mean
buildMachine :: Entropy -> Machine
buildMachine e = Machine sr dr kroot e
    where kroot = mkCCKernel $
                  kreassign : kgmu : kgsigma : map perturb ["c1","c2","c3"]

          kreassign = mkRDSLKernel ["scs"]
          kgmu      = mkGPKernel 0.05 ["mu"]
          kgsigma   = mkGPKernel 0.01 ["sigma"]
          perturb t = mkGPKernel 0.1 ["scs",t,"mu"]

main :: IO ()
main = run buildMachine 10 [ trace paramLocs, dump paramLocs ]
    where paramLocs = [ ([],"mu")
                      , ([],"sigma") ]