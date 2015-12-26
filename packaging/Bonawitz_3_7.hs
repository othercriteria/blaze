module Bonawitz_3_7
    where

import Math.Statistics.Inference.Blaze
      
-- Replicating instance of Bonawitz 3.7

sr :: State
sr = collectStates dummyState [sp, sa, sb, sx]

sp :: State
sp = mkDoubleParam "p" 0.3

sa :: State
sa = mkDoubleParam "alpha" 9.5

sb :: State
sb = mkDoubleParam "beta" 9.5

sx :: State
sx = mkIntData 1

dr :: Density
dr = productDensity [dbeta, dbin]

dbeta :: Density
dbeta = mkDensity [] [["p"]] [["alpha"], ["beta"]] beta

dbin :: Density
dbin = mkDensity [] [[""]] [["p"]] bernoulli

main = print $ density sr dr
