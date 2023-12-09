#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Either
import Text.Parsec
import Text.Parsec.Char

main :: IO ()
main = interact $ show . sum . map (extrapolate . makeHistory . parseline) . lines

extrapolate :: [[Int]] -> Int
extrapolate ls = head . solve [0] . reverse $ map head ls  --solve [0] . reverse  $ map head ls

solve :: [Int] -> [Int] -> [Int]
solve is     [    ] = is
solve (i:is) (h:hs) = solve ((h-i):is) hs

makeHistory :: [Int] -> [[Int]]
makeHistory is = makeHistory' is []

makeHistory' curr hist | all (==0) curr = hist ++ [curr]
makeHistory' curr hist =
    let diff = zipWith (-) (tail curr) curr
        hist' = hist ++ [curr]
    in makeHistory' diff hist'

parseline :: String -> [Int]
parseline =fromRight (error "parse fail") . parse round ""
    where
        round = (try neg <|> try pos) `sepBy` spaces
        neg = string "-" >> ((-1)*) . read <$> many1 digit
        pos = read <$> many1 digit
