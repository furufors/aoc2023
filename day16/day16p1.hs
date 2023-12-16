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
        pos1 = calc pos0 spd0
        (x1,y1) = pos1
        maxX = length (head m)
        maxY = length m
        e' = (energize (x1,y1) e)
        v' = S.insert (pos0,spd0) v
    in if 0 <= x1 && x1 < maxX && 0 <= y1 && y1 < maxY && not (S.member (pos0, spd0) v)
       then case m!!y1!!x1 of
            '.' -> run ((pos1, spd0):as) v' e' m
            '|' -> if spd0 == U || spd0 == D
                   then run ((pos1, spd0):as) v' e' m
                   else run ([(pos1, U),(pos1, D)] ++ as) v' e' m
            '/' -> case spd0 of
                    U -> run ((pos1, R):as) v' e' m
                    D -> run ((pos1, L):as) v' e' m
                    L -> run ((pos1, D):as) v' e' m
                    R -> run ((pos1, U):as) v' e' m
            '-' -> if spd0 == L || spd0 == R
                   then run ((pos1, spd0):as) v' e' m
                   else run ([(pos1, L),(pos1, R)] ++ as) v' e' m
            '\\' -> case spd0 of
                     U -> run ((pos1, L):as) v' e' m
                     D -> run ((pos1, R):as) v' e' m
                     L -> run ((pos1, U):as) v' e' m
                     R -> run ((pos1, D):as) v' e' m
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
