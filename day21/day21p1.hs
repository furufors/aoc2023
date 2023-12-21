#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import qualified Data.Set as S
type Point = (Int,Int)
type Points = S.Set Point

main :: IO ()
main = interact $ show . walk 64 . parse . lines

walk :: Int -> (Points, Int, Int, Points) -> Int
walk 0 (ps, _, _, _) = S.size ps
walk n (ps,mx,my,ss) =
    let next = S.fromList [p | (x,y) <- S.elems ps, (dx,dy) <- [(1,0),(0,1),(-1,0),(0,-1)]
                             , let y' = y + dy, let x' = x + dx, let p = (x',y')
                             , x' <= mx, 0 <= x'
                             , y' <= my, 0 <= y'
                             , not (p `S.member` ss) ]
    in walk (n-1) (next,mx,my,ss)

parse :: [[Char]] -> (Points, Int, Int, Points)
parse css = let maxY = length css - 1
                maxX = length (head css) - 1
                start = S.fromList [(x,y) | x <- [0..maxX], y <- [0..maxY], css!!y!!x == 'S']
                stones = S.fromList [(x,y) | x <- [0..maxX], y <- [0..maxY], css!!y!!x == '#']
            in (start, maxX, maxY, stones)
