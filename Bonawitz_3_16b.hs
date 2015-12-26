module Bonawitz_3_16b
    where

import Blaze
import Tree

-- Actual data with means generated from N(2,5)
-- > rnorm(6,2,5)
-- [1]  4.985808  3.624176 -9.130509  7.111286 -1.886614 -2.452056

-- > c(rnorm(2,5.0),rnorm(3,3.6),rnorm(4,-9.1),rnorm(5,7.1),
--   rnorm(6,-1.9),rnorm(7,-2.4))
--  [1]   5.0364076   4.8568302   4.5884737   2.5351775   3.1149254
--  [6] -10.0893287  -7.4426140  -9.3712219  -7.8065509   7.7228888
-- [11]   5.1626884   6.2366407   6.4889200   6.3024119  -4.0581073
-- [16]  -1.5525948  -2.6836409  -2.0571603  -3.1587941  -0.6084622
-- [21]  -3.5984489  -2.6619441  -2.4066728  -0.4383250  -2.2239350
-- [26]  -3.9156025  -2.6540578

sr :: State
sr = collectStates dummyState $ scs : sgps

sgps :: [State]
sgps = mkDoubleParams ["mu","sigma"] [1.0, 1.0]

scs :: State
scs = collectStates dummyCState (zipWith mkComp [1..] d) `tagNode` "scs"
    where d = [ "5.0 -10.1 5.2 -1.6 -3.6"
              , "-3.9 4.8 -7.4 6.2 -2.71"
              , "-2.72 -2.73 4.6 -9.3 6.4"
              , "-2.1 -2.4 2.5 -7.8 6.3"
              , "-3.2 -0.4 3.1 7.7"
              , "-4.0 -0.6 -2.2" ]

mkComp :: Int -> String -> State
mkComp i s = collectStates dummyState [mu,sigma,csx] `tagNode` ct
    where ct         = "c" ++ show i
          xs         = map read . words $ s
          csx        = collectStates dummyCState $
                       zipWith (flip tagNode) (((ct ++) . show) `fmap` [1..]) $
                       map mkDoubleData xs
          [mu,sigma] = mkDoubleParams ["mu","sigma"] [0.0,1.0]

dr :: Density
dr = mkACDensity (productDensity [dparam,dcomp]) ["scs"] [["mu"],["sigma"]]
     where dparam = mkDensity [] [["mu"]] [["dso","mu"],["dso","sigma"]] normal
           dcomp  = mkACDensity ddatum [""] [["mu"],["sigma"]]
           ddatum = mkDensity [] [[]] [["dso","mu"],["dso","sigma"]] normal

buildMachine :: Entropy -> Machine
buildMachine e = Machine sr dr kr e
    where kr = mkMHKernel $
               mkCMKernel [0.15,0.15,0.1,0.1,0.1,0.1,0.1,0.1,0.1] $
               (kgmu : kgsigma : map perturb comps) ++ [kreassign]

          kreassign = mkEGKernel $ mkRDSLKernel ["scs"]
          kgmu      = mkGPKernel 1.0 ["mu"]
          kgsigma   = mkGPKernel 1.0 ["sigma"]
          perturb t = mkGPKernel 0.5 ["scs",t,"mu"]
          comps     = map tagged . children $ getTagAt "scs" [] sr

main :: IO ()
main = run buildMachine 2000 [ trace paramLocs, dump paramLocs ]
    where paramLocs = [ ([],"mu")
                      , ([],"sigma") ]