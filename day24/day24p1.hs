#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List.Split
type Hail = ((Int,Int,Int),(Int,Int,Int))

main :: IO ()
main = interact $ show . run . map parse . lines

run :: [Hail] -> Int
run [    ] = 0
run [ h  ] = 0
run (h:hs) = sum [1 | h1 <- hs, collisionCourse h h1 200000000000000 400000000000000] + run hs

x, y, vx, vy, slope :: Hail -> Double
x ((px,_,_),(_,_,_)) = fromIntegral px
y ((_,py,_),(_,_,_)) = fromIntegral py
vx ((_,_,_),(pvx,_,_)) = fromIntegral pvx
vy ((_,_,_),(_,pvy,_)) = fromIntegral pvy
slope ((_,_,_),(vx,vy,_)) = (fromIntegral vy )/ (fromIntegral vx)

collisionCourse :: Hail -> Hail -> Double -> Double -> Bool
collisionCourse h1 h2 l h =
    if slope h1 == slope h2
    then False
    else let collx = ((slope h2 * x h2) - (slope h1 * x h1) + y h1 - y h2) / (slope h2 - slope h1)
             colly = (slope h1 * (collx - x h1)) + y h1
             future :: Hail -> Bool
             future o = all not [ (vx o) < 0 && (x o) < collx
                                , (vx o) > 0 && (x o) > collx
                                , (vy o) < 0 && (y o) < colly
                                , (vy o) > 0 && (y o) > colly
                                ]
         in if collx < l || collx > h || colly < l || colly > h
            then False
            else future h1 && future h2

parse :: String -> Hail
parse s = let (a,b) = span (/='@') s
              as = map read $ splitOn ", " a
              bs = map read $ splitOn ", " $ tail b
          in ((as!!0, as!!1, as!!2), (bs!!0, bs!!1, bs!!2))
