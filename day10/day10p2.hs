#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.List
type Pos = (Int,Int)
type Connected = M.Map Pos (Pos,Pos)

main :: IO ()
main = interact $ show . followPipes . toConnectedMap . lines

--(\path -> (abs (gaussArea path) - length path + 3) `div` 2)

gaussArea :: [Pos] -> Int
gaussArea [(x,y)] = 0
gaussArea ((x1,y1):(x2,y2):ps) = (y1 + y2) * (x2 - x1) + gaussArea ((x2,y2):ps)

followPipes :: (Pos, Connected) -> [Pos]
followPipes (start, mp) =
    let (x,y) = start
        possibleSecond = [(x+dx,y+dy) | dx <- [-1,0,1], dy <- [-1,0,1], (dx,dy) /= (0,0)]
    in last . sortBy (comparing length) . catMaybes . map (followTo []) $ zip (repeat start) possibleSecond
    where
        followTo :: [Pos] -> (Pos, Pos) -> Maybe [Pos]
        followTo hist (curr, next) | next == start = Just (curr:hist)
        followTo hist (curr, next) = case M.lookup next mp of
            Just (a,b) -> if a == curr then followTo (b:hist) (next, b) else followTo (a:hist) (next, a)
            Nothing -> Nothing

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
