module Bonawitz_3_10b
    where

import Blaze
import Tree

import Control.Monad.Random

-- Replicating instance of Bonawitz 3.10b

-- Dual Link Kernel

sr :: State
sr = collectStates dummyState [so, csc]

so :: State
so = mkDoubleParam "Omega" 1.23

csc :: State
csc = (collectStates dummyCState (map mkComp [0..2])) `tagNode` "cc"

mkComp :: Int -> State
mkComp i = (collectStates dummyCState [sp,csx]) `tagNode`
           ("c" ++ show i)
    where csx = collectStates dummyCState $ map mkIntData [i,i+1]
          sp  = mkDoubleParam "Theta" (0.2 * (fromIntegral i))

makeMachine :: (RandomGen g) => Rand g Machine
makeMachine = do
    e <- getRandoms 
    return $ Machine sr dummyDensity (mkRDDLKernel ["cc"] "c2" "c1") e
           
main :: IO ()
main = do
  m <- evalRandIO $ makeMachine

  let sim = iterate sampleMach m

  let out = putStrLn . show . map snd . summarizeComps . getTagAt "cc" [] . ms
            
  mapM_ out (take 100 sim)