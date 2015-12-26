module Bonawitz_3_19a
    where

import Blaze
import Tree

sr :: String -> State
sr s = collectStates dummyState $ (scs s) : sgps

sgps :: [State]
sgps = mkDoubleParams ["mu","sigma"] [1.0, 1.0]

scs :: String -> State
scs s = collectStates dummyCState comps `tagNode` "scs"
    where comps = map mkComp $ lines s

mkComp :: String -> State
mkComp s = collectStates dummyState [mu,sigma,csx] `tagNode` t
    where (t:ns)     = words s
          xs         = map read ns
          csx        = collectStates dummyCState $
                       zipWith (flip tagNode) (((t ++) . show) `fmap` [1..]) $
                       map mkDoubleData xs
          [mu,sigma] = mkDoubleParams ["mu","sigma"] [0.0,1.0]

dr :: Density
dr = mkACDensity (productDensity [dparam,dcomp]) ["scs"] [["mu"],["sigma"]]
     where dparam = mkDensity [] [["mu"]] [["dso","mu"],["dso","sigma"]] normal
           dcomp  = mkACDensity ddatum [""] [["mu"],["sigma"]]
           ddatum = mkDensity [] [[]] [["dso","mu"],["dso","sigma"]] normal

buildMachine :: String -> (Entropy -> Machine)
buildMachine s e = Machine sr' dr kr e
    where sr' = sr s

          kr = mkMHKernel $
               mkCMKernel [0.15,0.15,0.1,0.1,0.1,0.1,0.1,0.1,0.1] $
               (kgmu : kgsigma : map perturb comps) ++ [kreassign]

          kreassign = mkEGKernel $ mkRDSLKernel ["scs"]
          kgmu      = mkGPKernel 1.0 ["mu"]
          kgsigma   = mkGPKernel 1.0 ["sigma"]
          perturb t = mkGPKernel 0.5 ["scs",t,"mu"]
          comps     = map tagged . children $ getTagAt "scs" [] sr'

main :: IO ()
main = do
  s <- readFile "cluster.dat"

  let paramLocs = [ ([],"mu"), ([],"sigma") ]
  run (buildMachine s) 2000 [ trace paramLocs, burnin 500 $ dump paramLocs ]
