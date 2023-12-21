#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import qualified Data.Set as S
import qualified Data.Map as M
import Debug.Trace
type Point = (Int,Int)
type Stones = S.Set Point
type Points = M.Map Point [Point] -- For each Point, which universes it exists in.

main :: IO ()
main = interact $ show . walk 1000 . parse . lines

walk :: Int -> (Points, Int, Int, Stones) -> Int
walk 0 (ps, _, _, _) = sum (map length (M.elems ps))
walk n (ps,mx,my,ss) = trace (show (1000 - n) ++ ": " ++ (show . sum . map length $ (M.elems ps))) $
    let next = M.fromList [ (p, univ') | ((x,y),univ) <- M.assocs ps, (dx,dy) <- [(1,0),(0,1),(-1,0),(0,-1)]
                             , let y' = warp (y + dy) my, let x' = warp (x + dx) mx, let p = (x',y')
                             , let univ' = universe univ (x + dx) mx (y + dy) my
                             , not (p `S.member` ss) ]
    in walk (n-1) (next,mx,my,ss)

warp :: Int -> Int -> Int
warp x mx = let p = x `mod` mx in if x<0 then mx - x else x

universe :: [Point] -> Int -> Int -> Int -> Int -> [Point]
universe us x mx y my =
    let dx = if x > mx then 1 else if x < 0 then -1 else 0
        dy = if y > my then 1 else if y < 0 then -1 else 0
    in map (\(x,y) -> (x + dx, y + dy)) us

parse :: [[Char]] -> (Points, Int, Int, Stones)
parse css = let maxY = length css - 1
                maxX = length (head css) - 1
                start = M.fromList [ ((x,y),[(0,0)]) | x <- [0..maxX], y <- [0..maxY], css!!y!!x == 'S']
                stones = S.fromList [(x,y) | x <- [0..maxX], y <- [0..maxY], css!!y!!x == '#']
            in (start, maxX, maxY, stones)
