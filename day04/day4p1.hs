#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Either
import Text.Parsec
import Text.Parsec.Char

main :: IO ()
main = interact $ show . sum . map score . map parseline . lines

score :: (Int, [Int], [Int]) -> Int
score (i, hand, winners) =
    let whand = filter (\n -> n `elem` winners) hand
        count = length whand
    in if count == 0 then 0 else 2^(count - 1)

parseline :: String -> (Int, [Int], [Int])
parseline = fromRight (error "parse fail") . parse round ""
    where
        round = do
            string "Card" >> spaces
            i <- read <$> many1 digit
            string ": "
            hand <- many1 (noneOf "|") `sepBy`string "|"
            pure (i, parseNs . trim $ hand!!0, parseNs . trim $ hand!!1)
        trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

parseNs :: String -> [Int]
parseNs = fromRight (error "parse fail2") . parse pNs ""
    where
        pNs = (read <$> (spaces >> many1 digit)) `sepBy` string " "
