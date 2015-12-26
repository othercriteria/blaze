module Main
    where

import Blaze

import qualified Data.Map as M
import Data.Maybe
import Tree

-- R code for data generation
{-
ising <- matrix(-1,40,40)
for (i in 1:40) {
  for (j in 1:40) {
    if (sqrt((i-10)^2 + (j-15)^2) < 7) ising[i,j] = 1
  }
}
for (i in 1:40) {
  for (j in 1:40) {
    if (runif(1) < 0.08) ising[i,j] = -ising[i,j]
  }
}
image(ising)
write.table(ising,"~/ising_40.dat",sep=" ",col.names=FALSE,row.names=FALSE)
-}

type Coord = (Int,Int)

neighbors :: Coord -> [Coord]
neighbors (i,j) = [ (i-1,j), (i+1,j), (i,j-1), (i,j+1) ]

fromString :: String -> [(Coord,Double)]
fromString d = concatMap combine drow
    where dat  = map (map read . words) . lines $ d
          dcol = map (zip [1..]) dat
          drow = zip [1..] dcol

          combine (r,ds) = map (\(c,d) -> ((r,c),d)) ds

toString :: [(Coord,Double)] -> String
toString dat = table
    where (coords,_) = unzip dat
          (row,col)  = unzip coords
          [nr,nc]    = map maximum [row,col]

          m = M.fromList dat

          mkCell coord = maybe "NA" show $ M.lookup coord m
          mkRow r      = unwords $ map mkCell [ (r,c) | c <- [1..nc] ]
          table        = unlines $ map mkRow [1..nr]

toData :: Machine -> [(Coord,Double)]
toData (Machine sr _ _ _) = map (\c -> (read . tagged $ c, doubleVal $ c)) cs
    where cs = children $ getTA [""] sr

dumpIsing :: ReportAct
dumpIsing machines = do
  let name     i    = "ising_out/ising_" ++ show i ++ ".dat"
  let content    m  = toString . toData $ m
  let write   (i,m) = writeFile (name i) (content m)

  mapM_ write $ zip [0..] machines

  return machines

sr :: [(Coord,Double)] -> State
sr dat = collectStates dummyState $
         cond : spins : hyperj : hypert : hypers : spinp : params
    where spins          = dummyCState
                           `collectStates`
                           map mkSpins dat
          mkSpins (c,x)  = (mkSpin c `addDEdges` mkNeighbors c)
                           `collectStates`
                           [mkDoubleData x `tag` "h"]
          mkSpin (rn,cn) = mkDoubleData ((-1.0) ^ ((rn + cn) `mod` 2))
                           `tag`
                           show (rn,cn)
          mkNeighbors c  = map (\nc -> ["",(show nc)]) $
                           filter (flip elem allCoords) (neighbors c)
          allCoords      = map fst dat
                              
          params = mkDoubleParams ["j","temp"] [0.0,1.0]
          hyperj = flip tag "hj" $
                   collectStates dummyState $
                   mkDoubleParams ["mu","sigma"] [0.0,0.2]
          hypert = flip tag "ht" $
                   collectStates dummyState $
                   mkDoubleParams ["mu","sigma"] [0.0,0.3]

          spinp  = flip tag "spin" $
                   collectStates dummyCState $
                   mkDoubleParams (map show [0..]) [1.0,-1.2]
          hypers = flip tag "hs" $
                   collectStates dummyState $
                   mkDoubleParams ["mu","sigma"] [(-1.0),1.0]
          cond   = flip tag "cond" $
                   collectStates dummyState $
                   [mkTaggedData "fixed_spin" (DoubleDatum 1.0)]

dr :: Density
dr = productDensity [dcond,dj,dtemp,dspin,dgrid]
    where dcond = mkDensity [] [["spin","0"]] [["cond","fixed_spin"]] dirac

          dj    = mkDensity [] [["j"]] [["hj","mu"],["hj","sigma"]] normal
          dtemp = mkDensity [] [["temp"]] [["ht","mu"],["ht","sigma"]] lognormal

          dspin  = flip tag "dspin" $
                   mkACDensity cdspin ["spin"] [["hs","mu"],["hs","sigma"]]
          cdspin = mkDensity [] [[]] [["dso","mu"],["dso","sigma"]] normal
                  
          dgrid  = flip tag "dgrid" $
                   mkACDensity cdgrid [""] [["j"],["temp"],["spin"]]
          cdgrid = mkDensity [] [[]]
                   [["h"],["dso","j"],["dso","temp"],["dso","spin"]] ising
                                
buildMachine :: [(Coord,Double)] -> Entropy -> Machine
buildMachine dat e = Machine (sr dat) dr kr e
    where kr = mkCCKernel [kparam,kgrid]

          kparam = mkMHKernel $ mkCCKernel [kj,ktemp,kspins]
          kj     = mkGPKernel 0.1 ["j"]
          ktemp  = mkGPKernel 0.1 ["temp"]
          kspins = mkVCKernel ["spin"] ["dspin"] $
                   mkCMKernel [0.5,0.5] [mkIKernel,kspin]
          kspin  = mkMHKernel $
                   mkGPKernel 0.05 []
              
          kgrid = mkVCKernel [""] ["dgrid"] $
                  mkEGKernel $
                  mkDPKernel ["dso","spin"] []
                                
main :: IO ()
main = do
  dat <- fromString `fmap` readFile "ising_40.dat"
               
  let probeLocs = [ ([],"j"), ([],"temp"), (["spin"],"1")]
  foldl (>>=) (run (buildMachine dat)) [ stopAfter 2000
                                       , skim 20
                                       , trace probeLocs
                                       , burnin 10
                                       , dumpIsing
                                       , dump probeLocs
                                       ]
  putStrLn "Run complete!"
