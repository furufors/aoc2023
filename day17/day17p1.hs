#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Char
import Algorithm.Search
import Data.Maybe
data Dir = N | U | D | L | R deriving (Eq, Ord, Show)
type State = (Int,Int,Dir,Int) -- x,y,horiz,vert

main :: IO ()
main = interact $ show . fromJust . solve . map (map $ (\x -> read [x])) . lines

solve :: [[Int]] -> Maybe (Int, [State])
solve key = dijkstra next cost solved start
    where
        maxX = length (head key) - 1
        maxY = length key - 1
        solved :: State -> Bool
        solved (x,y,h,v) = (x,y) == (maxX, maxY)
        start :: State
        start = (0,0,N,0)
        cost :: State -> State -> Int
        cost _ (x,y,_,_) = key!!y!!x
        next :: State -> [State]
        next (x,y,d,n) = filter (valid d) [(x,y+1,D,(ct D d n))
                                          ,(x,y-1,U,(ct U d n))
                                          ,(x+1,y,R,(ct R d n))
                                          ,(x-1,y,L,(ct L d n))]
        ct :: Dir -> Dir -> Int -> Int
        ct d1 d2 n = if d1 == d2 then n + 1 else 1
        opposite :: Dir -> Dir -> Bool
        opposite U D = True
        opposite D U = True
        opposite L R = True
        opposite R L = True
        opposite a b = False
        valid :: Dir -> State -> Bool
        valid o (x,y,d,c) = x >= 0 && x <= maxX
                          && y >= 0 && y <= maxY
                          && c <= 3
                          && not (opposite o d)

