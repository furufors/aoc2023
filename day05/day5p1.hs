#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Either
import Data.List.Split (splitOn)
import Text.Parsec
import Text.Parsec.Char

main :: IO ()
main = interact $ show . minimum . mappings . parseInput . map lines . splitOn "\n\n"

mappings :: ([Int],[[(Int,Int,Int)]]) -> [Int]
mappings (ss, mps) = map (\s -> foldl applyMap s mps) ss

applyMap :: Int -> [(Int,Int,Int)] -> Int
applyMap i [] = i
applyMap i ((d,s,l):as) =
    if i >= s && i < s + l
    then i + d - s
    else applyMap i as

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
