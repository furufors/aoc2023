#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Either
import Data.List.Split (splitOn, chunksOf)
import Text.Parsec
import Text.Parsec.Char

main :: IO ()
main = interact $ show . fst . minimum . mappings . parseInput . map lines . splitOn "\n\n"

mappings :: ([Int],[[(Int,Int,Int)]]) -> [(Int,Int)]
mappings (ss, mps) =
    let pairs = zipWith ((,)) (first ss) (second ss)
        first [] = []
        first (x:xs) = x:second xs
        second [] = []
        second (x:xs) = first xs
        outer :: [(Int, Int)] -> [(Int,Int,Int)] -> [(Int, Int)]
        outer ps mp = concatMap (\p -> applyMap p mp) ps
    in foldl outer pairs mps

applyMap :: (Int,Int) -> [(Int,Int,Int)] -> [(Int,Int)]
applyMap i [] = [i]
applyMap (f,t) ((d,s,l):as) =
    if f <= s + l && s < f + t
    then before ++ interior ++ after
    else applyMap (f,t) as
    where
        before = if f < s then applyMap (f, s - f) as else []
        interior = [(d + max 0 (f - s), min t (l - max 0 (f - s)))]
        after = if s + l < f + t then applyMap (s + l, f + t - s - l) as else []

mapRange' :: (Int, Int) -> [MapItem] -> [(Int, Int)]
mapRange' x [] = [x]
mapRange' (rs, rl) (MapItem d s l : ms)
    | rs <= s + l && s < rs + rl = pre ++ curr ++ post
    | otherwise = mapRange' (rs, rl) ms
  where
    pre = if rs < s then mapRange' (rs, s - rs) ms else []
    curr = [(d + max 0 (rs - s), min rl (l - max 0 (rs - s)))]
    post = if s + l < rs + rl then mapRange' (s + l, rs + rl - s - l) ms else []

parseInput :: [[String]] -> ([Int],[[(Int,Int,Int)]])
parseInput s = (parseSeed . head $ head s, tail . map (map parseline . tail) $ s)

parseline :: String -> (Int, Int, Int)
parseline = fromRight (error "parse fail") . parse round ""
    where
        round = do
            a <- read <$> many1 digit
            b <- spaces >> read <$> many1 digit
            c <- spaces >> read <$> many1 digit
            pure (a,b,c)

parseSeed :: String -> [Int]
parseSeed = fromRight (error "parse fail seed,") . parse round ""
    where
        round = do
            string "seeds: "
            ss <- (read <$> many1 digit) `sepBy` string " "
            pure ss
