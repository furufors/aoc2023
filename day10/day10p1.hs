#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.List
type Pos = (Int,Int)
type Connected = M.Map Pos (Pos,Pos)

main :: IO ()
main = interact $ show . score . followPipes . toConnectedMap . lines

score :: [Pos] -> Int
score = (`div` 2) . length

followPipes :: (Pos, Connected) -> [Pos]
followPipes (start, mp) =
    let (x,y) = start
        possibleSecond = [(x+dx,y+dy) | dx <- [-1,0,1], dy <- [-1,0,1], (dx,dy) /= (0,0)]
    in last . sortBy (comparing length) . catMaybes . map (followTo [start]) $ zip (repeat start) possibleSecond
    where
        followTo :: [Pos] -> (Pos, Pos) -> Maybe [Pos]
        followTo hist (curr, next) | next == start = Just hist
        followTo hist (curr, next) = case M.lookup next mp of
            Just (a,b) | a == curr -> followTo (next:hist) (next, b)
            Just (a,b) | b == curr -> followTo (next:hist) (next, a)
            otherwise -> Nothing

toConnectedMap :: [[Char]] -> (Pos, Connected)
toConnectedMap css =
    let withPos = [(c,(x,y)) | (cs,y) <- zip css [0..], (c,x) <- zip cs [0..], c /= '.', c /= 'S']
        startPos = head $ [(x,y) | (cs,y) <- zip css [0..], (c,x) <- zip cs [0..], c == 'S']
    in (startPos, M.fromList $ map toNeighbours withPos)

toNeighbours :: (Char, Pos) -> (Pos,(Pos, Pos))
toNeighbours ('|', (x,y)) = ((x,y), ((x,y+1),(x,y-1)))
toNeighbours ('-', (x,y)) = ((x,y), ((x+1,y),(x-1,y)))
toNeighbours ('L', (x,y)) = ((x,y), ((x,y-1),(x+1,y)))
toNeighbours ('J', (x,y)) = ((x,y), ((x,y-1),(x-1,y)))
toNeighbours ('7', (x,y)) = ((x,y), ((x-1,y),(x,y+1)))
toNeighbours ('F', (x,y)) = ((x,y), ((x+1,y),(x,y+1)))
