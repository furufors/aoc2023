#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Debug.Trace
import Algorithm.Search
type Point = (Int,Int)
type PointType = Bool
type World = M.Map Point PointType
type Visited = S.Set Point
type State = (Point, Visited)

main :: IO ()
main = interact $ show . run1 . parse . lines

run1 :: World -> Int
run1 wd = let typeAt :: Point -> PointType
              typeAt p = case M.lookup p wd of
                Just  t -> t
                Nothing -> False
              start = let p = fst . head . filter (\(k,v) -> 0 == snd k && v) $ M.assocs wd in (p, S.fromList [p])
              exitY = snd . maximumBy (\a b -> compare (snd a) (snd b)) $ M.keys wd
              exit = fst . head . filter (\(k,v) -> exitY == snd k && v) $ M.assocs wd
              step ps = [ (p, S.insert p vs)
                        | ((x,y),vs) <- ps
                        , (dx,dy) <- [(0,1),(1,0),(0,-1),(-1,0)]
                        , let p = (x+dx,y+dy)
                        , not (S.member p vs)
                        , typeAt p]
              dive i p | fst p == exit = i
              dive i p = let next = step [p]
                         in if null next
                            then 0
                            else maximum $ map (dive (i+1)) next
          in trace (show start ++ " to " ++ show exit) $ dive 0 start

parse :: [String] -> World
parse ss = M.fromList $ concatMap (\(y,s) -> zipWith (\x l -> ((x,y), toPoint l)) [0..] s) $ zip [0..] ss

toPoint :: Char -> PointType
toPoint '.' = True
toPoint '>' = True
toPoint '^' = True
toPoint 'v' = True
toPoint '<' = True
toPoint '#' = False
