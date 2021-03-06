module Bonawitz_3_10c
    where

import Blaze
import Tree

import Control.Monad.Random

-- Replicating instance of Bonawitz 3.10b

-- Variation where a concrete cycle kernel consisting of three RDDL
-- kernels cycles the data points around in a ring.

sr :: State
sr = collectStates dummyState [so, csc]

so :: State
so = mkDoubleParam "Omega" 1.23

csc :: State
csc = (collectStates dummyCState (map mkComp [0..2])) `tagNode` "cc"

mkComp :: Int -> State
mkComp i = (collectStates dummyCState [sp,csx]) `tagNode`
           ("c" ++ show i)
    where csx = collectStates dummyCState $ map mkIntData [i,i+5]
          sp  = mkDoubleParam "Theta" (0.2 * (fromIntegral i))

k :: Kernel
k = mkCCKernel [ mkRDDLKernel ["cc"] "c0" "c1"
               , mkRDDLKernel ["cc"] "c1" "c2"
               , mkRDDLKernel ["cc"] "c2" "c0"
               ]
                
makeMachine :: (RandomGen g) => Rand g Machine
makeMachine = do
    e <- getRandoms 
    return $ Machine sr dummyDensity k e

main :: IO ()
main = do
  m <- evalRandIO $ makeMachine

  let sim = iterate sampleMach m
            
  mapM_ (putStrLn . show . ms) (take 10 sim)