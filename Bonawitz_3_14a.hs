module Bonawitz_3_14a
    where

import Blaze
import Tree

sr :: State
sr = collectStates dummyState $ (csx `tag` "csx"): sps

csx :: State
csx = collectStates dummyCState $
      zipWith mkTaggedData (map show [1..]) (map IntDatum [0,1,0,0,0,1,0])

sps :: [State]
sps = mkDoubleParams ["p","alpha","beta"] [0.2,1.5,1.5]

dr :: Density
dr = productDensity [dbin,dbeta]
    where dbeta = mkDensity [] [["p"]] [["alpha"],["beta"]] beta

          mkBernoulli s = mkDensity [] [["csx",tagged s]] [["p"]] bernoulli
          dbin          = productDensity $ map mkBernoulli (children csx)

buildMachine :: Entropy -> Machine
buildMachine e = Machine sr dr kr e
    where kr = mkGPKernel 0.02 ["p"]

main :: IO ()
main = run buildMachine 10 [trace [([],"p")]]