#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Maybe
type Point = (Int,Int)
type DirectNbrs = M.Map Point [Point]
type Nbrs = M.Map Point [(Point, Int)]

main :: IO ()
main = interact $ show . run . lines

run :: [String] -> Int
run ss =
    let points :: [(Int,Int)]
        points = [(i,j) | (j,r) <- zip [0..] (ss), (i,c) <- zip [0..] r, c /= '#']
        nbrs :: DirectNbrs
        nbrs = M.fromList [((x,y), [ p1 | (dx,dy) <- [(1,0),(-1,0),(0,1),(0,-1)], let p1 = (x+dx,y+dy), p1 `elem` points]) | (x,y) <- points]
        verts :: Nbrs
        verts = M.fromList [(p, [ warp p n 1 | n <- fromJust (M.lookup p nbrs) ]) | p <- points]
        warp :: Point -> Point -> Int -> (Point, Int)
        warp p n d = let ns = fromJust $ M.lookup n nbrs
                   in if (length ns == 2)
                      then warp n (head $ filter (/= p) ns) (d + 1)
                      else (n,d)
        loop n d b goal vis | n == goal = d
        loop n d b goal vis | S.member n vis = b
        loop n d b goal vis = maximum $ map (\(en,ed) -> loop en (d + ed) b goal (S.insert n vis)) (fromJust $ M.lookup n verts)
    in loop (head points) 0 0 (last points) S.empty
