#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List
type Pos = (Int,Int)

main :: IO ()
main = interact $ show . (\i -> manhattans (weights i) $ toCoords i) . lines

manhattans :: ([Int], [Int]) -> [Pos] -> Int
manhattans _ [    ] = 0
manhattans (xw, yw) (a:as) = sum [ manhattan a b | b <- as ] + manhattans (xw, yw) as
    where
        manhattan :: Pos -> Pos -> Int
        manhattan (a,b) (c,d) = vert + horiz
            where horiz = sum $ take (max a c - min a c) (drop (min a c) xw)
                  vert  = sum $ take (max b d - min b d) (drop (min b d) yw)

weights :: [String] -> ([Int], [Int])
weights ls = let f l = if all (=='.') l then 1000000 else 1 in (map f $ transpose ls, map f ls)

toCoords :: [String] -> [Pos]
toCoords ss =
    let xmax = length (head ss) - 1
        ymax = length ss - 1
    in [ (x,y) | x <- [0..xmax], y <- [0..ymax], let c = ss!!y!!x, c == '#' ]
