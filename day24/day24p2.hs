#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List.Split
import Math.LinearEqSolver
type Hail = ((Integer,Integer,Integer),(Integer,Integer,Integer))

main :: IO ()
main = do
    hails <- map parse . lines <$> getContents
    results <- run hails
    putStrLn $ results

run :: [Hail] -> IO (Maybe [Integer])
run hs = let ((x1,y1,z1),(vx1,vy1,vz1)) = head hs
             ((x2,y2,z2),(vx2,vy2,vz2)) = head .tail $ hs
             ((x3,y3,z3),(vx3,vy3,vz3)) = head . tail . tail $ hs
             coeffs = [
                      ,
                      ]
             vector = []
         in solveIntegerLinearEqs

parse :: String -> Hail
parse s = let (a,b) = span (/='@') s
              as = map read $ splitOn ", " a
              bs = map read $ splitOn ", " $ tail b
          in ((as!!0, as!!1, as!!2), (bs!!0, bs!!1, bs!!2))
