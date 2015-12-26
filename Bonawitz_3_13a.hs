module Bonawitz_3_13a
    where

import Blaze
import Tree
import Text.Printf
import Control.Monad.Random

sr :: State
sr = collectStates dummyState $ (csx `tagNode` "csx") : sps

csx :: State
csx = collectStates dummyCState $
      zipWith mkTaggedData (map show [1..]) (map DoubleDatum d)
          where d = [(-0.8), (-1.3), 2.0, (-1.8), 0.0, 2.7, 0.2, (-0.3)]

sps :: [State]
sps = mkDoubleParams ["mu","sigma"] [1.0,1.0]

sd :: Density
sd = productDensity [dmu, dsigma, dnorm]
    where dmu    = mkDensity [] [["mu"]]    [] vague
          dsigma = mkDensity [] [["sigma"]] [] vaguePositive

          mkNormal t = mkDensity [] [["csx",t]] [["mu"],["sigma"]] normal
          dnorm      = productDensity $ map mkNormal (map tagged $ children csx)

makeMachine :: Entropy -> Machine
makeMachine e = Machine sr sd kroot e
    where kmu    = mkGPKernel 0.1 ["mu"]
          ksigma = mkGPKernel 0.1 ["sigma"]

          kroot  = mkCCKernel [kmu,ksigma]

main :: IO ()
main = do
  e <- evalRandIO $ getRandoms

  let m   = makeMachine e
  let sim = iterate sampleMach m

  let trace m = printf "(%f,%f): %s" (doubleVal . getTagAt "mu" [] . ms $ m)
                (doubleVal . getTagAt "sigma" [] . ms $ m)
                (show . topDensity $ m)

  mapM_ (putStrLn . trace) (take 50 sim)
  
