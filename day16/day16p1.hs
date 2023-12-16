#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import qualified Data.Map as M
import qualified Data.Set as S
type Energy = M.Map (Int,Int) Int
data Dir = U | D | L | R deriving (Eq,Ord)
type Visited = S.Set ((Int,Int),Dir)

main :: IO ()
main = interact $ show . run [((-1,0),R)] S.empty M.empty . lines

run :: [((Int,Int),Dir)] -> Visited -> Energy -> [[Char]] -> Int
run [] v e m = length . filter (>0) $ M.elems e
run (a:as) v e m =
    let (pos0, spd0) = a
        pos1@(x1,y1) = calc pos0 spd0
        maxX = length (head m)
        maxY = length m
        e' = (energize (x1,y1) e)
        v' = S.insert (pos0,spd0) v
        next = case m!!y1!!x1 of
                '.' -> (pos1, spd0):as
                '|' -> if spd0 == U || spd0 == D
                       then (pos1, spd0):as
                       else [(pos1, U),(pos1, D)] ++ as
                '/' -> case spd0 of
                        U -> (pos1, R):as
                        D -> (pos1, L):as
                        L -> (pos1, D):as
                        R -> (pos1, U):as
                '-' -> if spd0 == L || spd0 == R
                       then (pos1, spd0):as
                       else [(pos1, L),(pos1, R)] ++ as
                '\\' -> case spd0 of
                         U -> (pos1, L):as
                         D -> (pos1, R):as
                         L -> (pos1, U):as
                         R -> (pos1, D):as
    in if 0 <= x1 && x1 < maxX && 0 <= y1 && y1 < maxY && not (S.member (pos0, spd0) v)
       then run next v' e' m
       else run as v e m

energize :: (Int,Int) -> Energy -> Energy
energize p e = case M.lookup p e of
    Nothing -> M.insert p 1 e
    Just a -> M.insert p (a+1) e

calc :: (Int,Int) -> Dir -> (Int,Int)
calc (a,b) U = (a, b - 1)
calc (a,b) D = (a, b + 1)
calc (a,b) R = (a + 1, b)
calc (a,b) L = (a - 1, b)
