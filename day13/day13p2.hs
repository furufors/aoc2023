#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List
import Data.List.Split

main :: IO ()
main = interact $ show . sum . map (findMirror . lines) . splitOn "\n\n"

findMirror :: [[Char]] -> Int
findMirror m = let horiz = findMirror' (transpose m)
                   vert = findMirror' m
                   findMirror' m = let r = length (head m) - 1
                                       cond :: Int -> Bool
                                       cond n = (==1) . sum $ map (\row -> length . filter not $ zipWith (==) (reverse $ take n row) (drop n row)) m
                                   in [ i |  i <- [1..r], cond i]
               in case (horiz,vert) of
                       ([  ], [  ]) -> 0
                       (a:as,    _) -> 100 * a
                       (_   , b:bs) -> b
