module Bonawitz_3_8
    where

import Blaze
import Tree
    
-- Replicating instance of Bonawitz 3.8
-- In addition, the tempering density described on pg. 48 is used

sr :: State
sr = collectStates dummyState $
     (scs `tagNode` "cs") : mkDoubleParams ["p","alpha","beta"] [0.3,1.5,1.5]

scs :: State
scs = collectStates dummyCState $
      zipWith (\t v -> mkIntData v `tagNode` ("x" ++ show t)) [1..] [0,0,1]

dr :: Density
dr = productDensity [dbeta, dbin]

dbeta :: Density
dbeta = mkDensity [] [["p"]] [["alpha"], ["beta"]] beta

dbin :: Density
dbin = productDensity $ map mkBernoulli (map tagged $ children scs)
    where mkBernoulli t = mkDensity [] [["cs",t]] [["p"]] bernoulli

tempered :: Double -> Prob
tempered tau = density s' (mkDensity [dr] [] [["tau"]] tempering)
    where s' = collectStates sr [mkDoubleParam "tau" tau]
                          
main = print $ map tempered [0.01,0.1,1.0,10.0,100.0]
              