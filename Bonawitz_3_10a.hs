module Bonawitz_3_10a
    where

import Blaze
import Tree

import Control.Monad.Random

-- Replicating instance of Bonawitz 3.10a

-- Single Link Kernel

sr :: State
sr = collectStates dummyState [so, csc]

so :: State
so = mkDoubleParam "Omega" 1.23

csc :: State
csc = (collectStates dummyCState (map mkComp [0..4])) `tagNode` "cc"

mkComp :: Int -> State
mkComp i = (collectStates dummyState [sp,csx]) `tagNode`
           ("c" ++ show i)
    where csx = collectStates dummyCState $ map mkIntData [i,i+5]
          sp  = mkDoubleParam "Theta" (0.2 * (fromIntegral i))

makeMachine :: (RandomGen g) => Rand g Machine
makeMachine = do
    e <- getRandoms 
    return $ Machine sr dummyDensity (mkRDSLKernel ["cc"]) e
           
main :: IO ()
main = do
  m <- evalRandIO $ makeMachine

  let sim = iterate sampleMach m

  let out = putStrLn . show . map snd . summarizeComps . getTagAt "cc" [] . ms
            
  mapM_ out (take 100 sim)