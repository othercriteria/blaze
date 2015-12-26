module Main
    where

import Blaze
    
import qualified Data.Map as M
import Data.Maybe
import Tree
    
-- R code for data generation
{-
ising <- matrix(-1,12,12)
for (i in 1:12) {
  for (j in 1:12) {
    if (sqrt((i-6)^2 + (j-8)^2) < 3.5) ising[i,j] = 1
  }
}
for (i in 1:12) {
  for (j in 1:12) {
    if (runif(1) < 0.1) ising[i,j] = -ising[i,j]
  }
}
image(ising)
write.table(ising,"~/ising.dat",sep=" ",col.names=FALSE,row.names=FALSE)
-}

type Coord = (Int,Int)

neighbors :: Coord -> [Coord]
neighbors (i,j) = [ (i-1,j), (i+1,j), (i,j-1), (i,j+1) ]
    
fromString :: String -> [(Coord,Int)]
fromString d = concatMap combine drow
    where dat  = map (map read . words) . lines $ d
          dcol = map (zip [1..]) dat
          drow = zip [1..] dcol

          combine (r,ds) = map (\(c,d) -> ((r,c),d)) ds

toString :: [(Coord,Int)] -> String
toString dat = table
    where (coords,_) = unzip dat
          (row,col)  = unzip coords
          [nr,nc]    = map maximum [row,col]

          m = M.fromList dat

          mkCell coord = maybe "NA" show $ M.lookup coord m
          mkRow r      = unwords $ map mkCell [ (r,c) | c <- [1..nc] ]
          table        = unlines $ map mkRow [1..nr]

toData :: Machine -> [(Coord,Int)]
toData (Machine sr _ _ _) = map (\c -> (read . tagged $ c, intVal $ c)) cs
    where cs = children $ getTA [""] sr

dumpIsing :: ReportAct
dumpIsing machines = do
  let name     i    = "ising_out/ising_" ++ show i ++ ".dat"
  let content    m  = toString . toData $ m
  let write   (i,m) = writeFile (name i) (content m)

  mapM_ write $ zip [0..] machines

  return machines
               
sr :: [(Coord,Int)] -> State
sr dat = collectStates dummyState $ ssmooth : params
    where ssmooth        = collectStates dummyCState $ map mkSmooth dat
          mkSmooth (c,x) = collectStates (mkCell c `addDEdges` mkNeighbors c) $
                           [mkDoubleData (fromIntegral x) `tag` "h"]
          mkCell (rn,cn) = mkIntData ((-1) ^ ((rn + cn) `mod` 2))
                           `tag` (show (rn,cn))
          mkNeighbors c  = map (\nc -> ["",(show nc)]) $
                           filter (flip elem allCoords) (neighbors c)
          allCoords      = map fst dat
                              
          params = mkDoubleParams ["j","temp"] [0.4,0.3]

dr :: Density
dr = mkACDensity cd [""] [["j"],["temp"]]
    where cd = mkDensity [] [[]] [["h"],["dso","j"],["dso","temp"]] ising
                                
buildMachine :: [(Coord,Int)] -> Entropy -> Machine
buildMachine dat e = Machine (sr dat) dr kr e
    where kr      = mkVCKernel [""] [] $ mkEGKernel $
                    mkDPKernel choices []
          choices = map IntDatum [(-1),1]
                                
main :: IO ()
main = do
  dat <- fromString `fmap` readFile "ising_40.dat"
               
  let probeLocs = [ ([""],"(1,1)"), ([""],"(1,2)"), ([""],"(1,3)") ]
  foldl (>>=) (run (buildMachine dat)) [ stopAfter 5
                                       , dumpIsing
                                       ]
  putStrLn "Run complete!"
