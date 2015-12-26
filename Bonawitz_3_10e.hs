module Bonawitz_3_10e
    where

import Blaze
import Tree

import Control.Monad.Random

-- Replicating instance of Bonawitz 3.10b

-- Test of Conditional Hybrid Kernel

sr :: State
sr = collectStates dummyState [so, csc]

so :: State
so = mkDoubleParam "Omega" 1.23

csc :: State
csc = (collectStates dummyCState (map mkComp [0..2])) `tagNode` "cc"

mkComp :: Int -> State
mkComp i = (collectStates dummyState [sp,csx]) `tagNode`
           ("c" ++ show i)
    where csx = collectStates dummyCState $ map mkIntData [i,i+5]
          sp  = mkDoubleParam "Theta" (0.2 * (fromIntegral i))

k :: Kernel
k = mkCHKernel ["cc"] select' [ mkRDDLKernel ["cc"] "c2" "c0" `tagNode` "p1"
                              , mkIKernel                     `tagNode` "p2"
                              ]

select' :: State -> Tag
select' s | (any (\c -> isCompEmpty $ getTagAt c [] s) ["c0","c1","c2"]) = "p2"
          | otherwise                                                    = "p1"
              
makeMachine :: (RandomGen g) => Rand g Machine
makeMachine = do
    e <- getRandoms
    return $ Machine sr dummyDensity k e
                             
main :: IO ()
main = do
  m <- evalRandIO $ makeMachine

  let sim = iterate sampleMach m

  let out = putStrLn . show . map snd . summarizeComps . getTagAt "cc" [] . ms
            
  mapM_ out (take 6 sim)