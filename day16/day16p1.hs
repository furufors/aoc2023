#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import qualified Data.Set as S
data Dir = U | D | L | R deriving (Eq,Ord)
type Energized = S.Set (Int,Int)
type Visited = S.Set ((Int,Int),Dir)

main :: IO ()
main = interact $ show . run [((-1,0),R)] S.empty S.empty . lines

run :: [((Int,Int),Dir)] -> Visited -> Energized -> [[Char]] -> Int
run [] v e m = S.size e
run (a:as) v e m =
    let (pos0, dir) = a
        pos1@(x1,y1) = calc pos0 dir
        e' = S.insert pos1 e
        v' = S.insert (pos0, dir) v
        next = case m!!y1!!x1 of
                '.' -> (pos1, dir):as
                '|' -> if dir == U || dir == D
                       then (pos1, dir):as
                       else [(pos1, U),(pos1, D)] ++ as
                '/' -> case dir of
                        U -> (pos1, R):as
                        D -> (pos1, L):as
                        L -> (pos1, D):as
                        R -> (pos1, U):as
                '-' -> if dir == L || dir == R
                       then (pos1, dir):as
                       else [(pos1, L),(pos1, R)] ++ as
                '\\' -> case dir of
                         U -> (pos1, L):as
                         D -> (pos1, R):as
                         L -> (pos1, U):as
                         R -> (pos1, D):as
    in if 0 <= x1 && x1 < (length (head m)) && 0 <= y1 && y1 < (length m) && not (S.member (pos0, dir) v)
       then run next v' e' m
       else run as v e m

calc :: (Int,Int) -> Dir -> (Int,Int)
calc (a,b) U = (a, b - 1)
calc (a,b) D = (a, b + 1)
calc (a,b) R = (a + 1, b)
calc (a,b) L = (a - 1, b)
