#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

main :: IO ()
main = interact $ show . sumGears . toAdjacents . lines

sumGears :: [(Int, [(Int,Int)])] -> Int
sumGears [] = 0
sumGears ((i,ps):rs) = sum [i * x | star <- ps, (x, os) <- rs, ostar <- os, star == ostar] + sumGears rs

toAdjacents :: [[Char]] -> [(Int, [(Int,Int)])]
toAdjacents css =
    let height = length css
        width = length (head css)
    in toA width height 0 0 []
    where
        toA :: Int -> Int -> Int -> Int -> [(Int, [(Int,Int)])] -> [(Int, [(Int,Int)])]
        toA w h x y acc | x + 1 >= w && y + 1 >= h = acc
        toA w h x y acc | x + 1 >= w = toA w h 0 (y + 1) acc
        toA w h x y acc = if css!!y!!x `elem` ['0'..'9']
                          then let str = drop x (css!!y)
                                   i = takeWhile (`elem` ['0'..'9']) str
                                   l = length i
                                   ns = [ (x,y) | y <- [y-1, y, y+1], x <-[(x-1)..(x+l)], x >= 0, x < w, y >= 0, y < h, css!!y!!x == '*']
                                   acc' =  if length ns > 0 then ((read i, ns):acc) else acc
                               in toA w h (x+l) y acc'
                          else toA w h (x+1) y acc

