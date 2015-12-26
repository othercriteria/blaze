module Bonawitz_3_21b
    where

import Blaze

sr :: String -> State
sr s = collectStates dummyState $ (scs s) : sgps

sgps :: [State]
sgps = mkDoubleParams ["mu","sigma"] [1.0, 1.0]

scs :: String -> State
scs s = collectStates dummyCState comps `tag` "scs"
    where comps = map mkComp $ lines s

mkComp :: String -> State
mkComp s = collectStates dummyState (csx : params) `tag` t
    where (t:ns)     = words s
          csx        = collectStates dummyCState $
                       zipWith (flip tag) (((t ++) . show) `fmap` [1..]) $
                       map (mkDoubleData . read) ns
          params     = mkDoubleParams ["mu","sigma"] [0.0,1.0]

dr :: Density
dr = mkACDensity (productDensity [dparam,dcomp]) ["scs"] [["mu"],["sigma"]]
     where dparam = mkDensity [] [["mu"]] [["dso","mu"],["dso","sigma"]] normal
           dcomp  = mkACDensity ddatum [""] [["mu"],["sigma"]]
           ddatum = mkDensity [] [[]] [["dso","mu"],["dso","sigma"]] normal

buildMachine :: String -> (Entropy -> Machine)
buildMachine s e = Machine sr' dr kr e
    where sr' = sr s

          kr = mkMHKernel $
               mkCMKernel [0.4,0.48,0.112,0.008] $
               [kglobal,kcomp,kreassign,kreassign']

          kgmu       = mkGPKernel 0.5 ["mu"]
          kgsigma    = mkGPKernel 2.0 ["sigma"]
          kglobal    = mkCCKernel [kgmu,kgsigma]
          kcomp      = mkVCKernel ["scs"] $ \ts -> mkGPKernel 0.4 (ts ++ ["mu"])
          kreassign  = mkEGKernel $ mkRDSLKernel ["scs"]
          kreassign' = mkEGKernel $ mkCCKernel [kreassign,kreassign]

main :: IO ()
main = do
  s <- readFile "cluster.dat"

  let paramLocs = [ ([],"mu"), ([],"sigma") ]
  run (buildMachine s) 2000 [ trace paramLocs, burnin 500 $ dump paramLocs ]
