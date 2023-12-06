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
    let pairs = zipWith (\a b -> (a, a + b)) (first ss) (second ss)
        first [] = []
        first (x:xs) = x:second xs
        second [] = []
        second (x:xs) = first xs
        outer ps mp = concatMap (\p -> applyMap p mp) ps
    in foldl outer pairs mps

applyMap :: (Int,Int) -> [(Int,Int,Int)] -> [(Int,Int)]
applyMap (a,b) [          ] = [(a,b)]
applyMap (a,b) ((d,s,l):as) =
    if b < sa || a >= sb
    then applyMap (a,b) as
    else before ++ interior ++ after
    where
        (sa, sb) = (s, s + l)
        before = if a < sa then applyMap (a, sa - 1) as else []
        interior = [(d + max 0 (a - sa), d + min l (b - sa))]
        after = if b >= sb then applyMap (sb, b) as else []

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
