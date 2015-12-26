module Bonawitz_3_18b
    where

import Blaze
import Tree

sr :: State
sr = collectStates dummyState $ csx : sps

-- Real data from N(3,12)
csx :: State
csx = collectStates dummyCState $
      zipWith (flip tagNode) (show `fmap` [1..]) $
      map (mkDoubleData . read) $
      words "25.8 11.3 -12.7 -3.2 -29.6 2.1 21.8 -5.2 14.2 -2.7 -0.7 -7.1 18.1"

sps :: [State]
sps = mkDoubleParams ["mu","sigma"] [1.0,1.0]

dr :: Density
dr = productDensity [dmu,dsigma,dnorm]
    where dmu    = mkDensity [] [["mu"]]    [] vague
          dsigma = mkDensity [] [["sigma"]] [] vaguePositive
          dnorm  = mkACDensity cd [""] [["mu"],["sigma"]]
          cd     = mkDensity [] [[]] [["dso","mu"],["dso","sigma"]] normal

buildMachine :: Entropy -> Machine
buildMachine e = Machine sr dr kr e
    where kmu    = mkGPKernel 1.0 ["mu"]
          ksigma = mkGPKernel 0.5 ["sigma"]

          kr = mkMHKernel $ mkCCKernel [kmu,ksigma]

main :: IO ()
main = run buildMachine 5000 [ trace paramLocs, burnin 500 $ dump paramLocs ]
    where paramLocs = [ ([],"mu")
                      , ([],"sigma") ]