#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List
import Data.Maybe
import qualified Data.Map as M
type Hist = M.Map [String] Int

main :: IO ()
main = interact $ show . calcLoad . findLarge . run 0 M.empty . lines

run :: Int -> Hist -> [[Char]] -> (Int,Int,Hist)
run n h m = let next = cyc m
            in case M.lookup next h of
                Nothing -> run (n+1) (M.insert next (n+1) h) next
                Just a  -> (a, n+1, h)

findLarge :: (Int,Int,Hist) -> [[Char]]
findLarge (a,b,h) = let cycLen = b - a
                        after = (1000000000 - b) `mod` cycLen
                        target = b - cycLen + after
                    in fst . fromJust $ find (\(_,n) -> n == target) (M.assocs h)

cyc :: [[Char]] -> [[Char]]
cyc = tiltE . tiltS . tiltW . tiltN
    where
        tiltN = transpose . map fall . transpose
        tiltW = map fall
        tiltS = transpose . map reverse . map fall . map reverse . transpose
        tiltE = map reverse . map fall . map reverse

fall :: [Char] -> [Char]
fall []       = []
fall ('#':cs) = '#':fall cs
fall cs       = let (section, rest) = span (/='#') cs
                    fallen = filter (=='O') section ++ filter (=='.') section
                in fallen ++ fall rest

calcLoad :: [[Char]] -> Int
calcLoad m = sum . map (\(i,row) -> i * (length $ filter (== 'O') row)) $ zip (reverse $ [1..(length m)]) m
