#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List.Split
import Data.Set
type Visited = S.Set Int
type Hail = ((Int,Int,Int),(Int,Int,Int))

main :: IO ()
main = interact $ show . run . map parse . zip [0..] . lines

run :: [(Int,Hail)] -> Int
run hs = let remaining = S.fromList $ map fst hs
             startPos = map (fst . snd) hs
         in loop candidates remaining it1 it2

timeStepAll :: [(Int,Hail)] -> [(Int,Hail)]
timeStepAll = map (\(i,h) -> (i, timeStep h))

timeStep :: Hail -> Hail
timeStep ((x,y,z),(vx,vy,vz)) = ((x + vx,y + vy,z + vz),(vx,vy,vz))

x, y, vx, vy, slope :: Hail -> Double
x ((px,_,_),(_,_,_)) = fromIntegral px
y ((_,py,_),(_,_,_)) = fromIntegral py
vx ((_,_,_),(pvx,_,_)) = fromIntegral pvx
vy ((_,_,_),(_,pvy,_)) = fromIntegral pvy
slope ((_,_,_),(vx,vy,_)) = (fromIntegral vy )/ (fromIntegral vx)

parse :: String -> (Int, Hail)
parse (i,s) = let (a,b) = span (/='@') s
                  as = map read $ splitOn ", " a
                  bs = map read $ splitOn ", " $ tail b
              in (i,((as!!0, as!!1, as!!2), (bs!!0, bs!!1, bs!!2)))
