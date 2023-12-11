#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List
type Pos = (Int,Int)

main :: IO ()
main = interact $ show . sum . (\i -> manhattans i $ toCoords i) . lines

manhattans :: [String] -> [Pos] -> [Int]
manhattans ss [    ] = []
manhattans ss (a:as) = [ manhattan a b | b <- as ] ++ manhattans ss as
    where
        manhattan :: Pos -> Pos -> Int
        manhattan (a,b) (c,d) =
            abs (a-c) + abs (b-d) + emptyBetween b d ss + emptyBetween a c (transpose ss)

emptyBetween :: Int -> Int -> [String] -> Int
emptyBetween a b ss =
    let u = max a b
        l = min a b
        d = u - l
    in (*(999999)) . length . filter (all (== '.')) . take (d-1) . drop (l+1) $ ss

toCoords :: [String] -> [Pos]
toCoords ss =
    let xmax = length (head ss) - 1
        ymax = length ss - 1
    in [ (x,y) | x <- [0..xmax], y <- [0..ymax], let c = ss!!y!!x, c == '#' ]
