#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Algorithm.Search
type Point = (Int,Int)
data PointType = Empty | RSlope | USlope | DSlope | LSlope | Blocked deriving (Show, Eq)
type World = M.Map Point PointType
type Visited = S.Set Point
type State = (Point, Visited)

main :: IO ()
main = interact $ show . run1 . parse . lines

run1 :: World -> [Int]
run1 wd = let typeAt :: Point -> PointType
              typeAt p = case M.lookup p wd of
                Just  t -> t
                Nothing -> Blocked
              start = let p = fst . head . filter (\(k,v) -> 0 == snd k && v == Empty) $ M.assocs wd in [(p, S.fromList [p])]
              step ps = [ (p, S.insert p vs)
                        | ((x,y),vs) <- ps
                        , (dx,dy) <- [(0,1),(1,0),(0,-1),(-1,0)]
                        , let p = (x+dx,y+dy)
                        , not (S.member p vs)
                        , okMove (dx,dy) (typeAt p)]
              exitY = snd . maximumBy (\a b -> compare (snd a) (snd b)) $ M.keys wd
              exit = fst . head . filter (\(k,v) -> exitY == snd k && v == Empty) $ M.assocs wd
              loop i ns ps =  if null ps
                              then ns
                              else if any (\(p,s) -> p == exit) ps
                                   then loop (i+1) (i:ns) (step ps)
                                   else loop (i+1) ns (step ps)
          in loop 0 [] start

okMove :: Point -> PointType -> Bool
okMove _ Empty = True
okMove (1, 0) RSlope = True
okMove (-1,0) LSlope = True
okMove (0, 1) DSlope = True
okMove (0,-1) USlope = True
okMove _ _ = False

parse :: [String] -> World
parse ss = M.fromList $ concatMap (\(y,s) -> zipWith (\x l -> ((x,y), toPoint l)) [0..] s) $ zip [0..] ss

toPoint :: Char -> PointType
toPoint '.' = Empty
toPoint '>' = RSlope
toPoint '^' = USlope
toPoint 'v' = DSlope
toPoint '<' = LSlope
toPoint '#' = Blocked
