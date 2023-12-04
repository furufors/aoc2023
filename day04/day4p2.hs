#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Either
import Text.Parsec
import Text.Parsec.Char

main :: IO ()
main = interact $ show . numCards . map (score . parseline) . lines

numCards :: [Int] -> Int
numCards = fst . foldl fun (0, repeat 1)
    where
        fun (count, a:as) score = (count + a, zipWith (+) (replicate score a ++ [0, 0 ..]) as)

score :: (Int, [Int], [Int]) -> Int
score (i, hand, winners) =
    let whand = filter (\n -> n `elem` winners) hand
        count = length whand
    in count

parseline :: String -> (Int, [Int], [Int])
parseline = fromRight (error "parse fail") . parse round ""
    where
        round = do
            _ <- string "Card "
            _ <- spaces
            i <- read <$> many1 digit
            _ <- string ": "
            hand <- many1 (noneOf "|") `sepBy`string "|"
            pure (i, parseNs . trim $ hand!!0, parseNs . trim $ hand!!1)

parseNs :: String -> [Int]
parseNs = fromRight (error "parse fail2") . parse pNs ""
    where
        pNs = do
            --_ <- spaces
            l <- (read <$> (spaces >> many1 digit)) `sepBy` string " "
            return l

trim xs = dropSpaceTail "" $ dropWhile (== ' ') xs

dropSpaceTail maybeStuff "" = ""
dropSpaceTail maybeStuff (x:xs)
        | x == ' ' = dropSpaceTail (x:maybeStuff) xs
        | null maybeStuff = x : dropSpaceTail "" xs
        | otherwise       = reverse maybeStuff ++ x : dropSpaceTail "" xs
