#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List
type Pos = (Int,Int)

main :: IO ()
main = interact $ show . sum . manhattans . toCoords . expand . lines


manhattans :: [Pos] -> [Int]
manhattans [] = []
manhattans (a:as) = [ manhattan a b | b <- as ] ++ manhattans as

manhattan :: Pos -> Pos -> Int
manhattan (a,b) (c,d) = abs (a-c) + abs (b-d)

toCoords :: [String] -> [Pos]
toCoords ss =
    let xmax = length (head ss) - 1
        ymax = length ss - 1
    in [ (x,y) | x <- [0..xmax], y <- [0..ymax], let c = ss!!y!!x, c == '#' ]

expand :: [String] -> [String]
expand = transpose . doubleEmpty . transpose . doubleEmpty

doubleEmpty :: [String] -> [String]
doubleEmpty [] = []
doubleEmpty (a:as) | all (== '.') a = a:a:(doubleEmpty as)
doubleEmpty (a:as) = a:(doubleEmpty as)
