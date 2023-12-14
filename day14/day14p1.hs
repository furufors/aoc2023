#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List

main :: IO ()
main = interact $ show . calcLoad . tiltN . lines

tiltN :: [[Char]] -> [[Char]]
tiltN = transpose . map fall . transpose

fall :: [Char] -> [Char]
fall []       = []
fall ('#':cs) = '#':fall cs
fall cs       = let (section, rest) = span (/='#') cs
                    fallen = filter (=='O') section ++ filter (=='.') section
                in fallen ++ fall rest

calcLoad :: [[Char]] -> Int
calcLoad m = sum . map (\(i,row) -> i * (length $ filter (== 'O') row)) $ zip (reverse $ [1..(length m)]) m
