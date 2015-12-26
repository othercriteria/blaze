module Bonawitz_3_9
    where

import Blaze
import Tree
    
-- Replicating instance of Bonawitz 3.9

sr :: State
sr = collectStates dummyState $
     (csx `tagNode` "csx") : mkDoubleParams ["p","alpha","beta"] [0.3,1.5,1.5]

csx :: State
csx = collectStates dummyCState $
      zipWith mkTaggedData (map show [1..]) (map IntDatum [0, 0, 1])

dr :: Density
dr = productDensity [dbeta, dbin]
    where dbeta = mkDensity [] [["p"]] [["alpha"],["beta"]] beta

          mkBernoulli t = mkDensity [] [["csx",t]] [["p"]] bernoulli
          dbin          = productDensity $
                          map mkBernoulli (map tagged $ children csx)

k :: Kernel
k = mkGPKernel 0.02 ["p"]

e :: Entropy
e = [0.2,0.9,0.6,0.1,0.8,0.95]

m :: Machine
m = Machine sr dr k e

report :: Machine -> String
report m = show (topDensity m) ++ show (ms m)

main :: IO ()           
main = do
  putStrLn . report $ m
  let m' = sampleMach m
  putStrLn . report $ m'
  let m'' = sampleMach m'
  putStrLn . report $ m''
