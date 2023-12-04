#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Debug.Trace
import Data.Either
import Text.Parsec
import Text.Parsec.Char

main :: IO ()
main = interact $ show . sum . map score .  map parseline . lines

score :: (Int, [Int], [Int]) -> Int
score (i, hand, winners) =
    let whand = filter (\n -> n `elem` winners) hand
        count = length whand
    in if count == 0 then 0 else 2^(count - 1)

parseline :: String -> (Int, [Int], [Int])
parseline = fromRight (error "parse fail") . parse round ""
    where
        round = do
            _ <- string "Card "
            _ <- spaces
            i <- read <$> many1 digit
            _ <- string ": "
            hand <- many1 (noneOf "|") `sepBy`string "|"
            --_ <- string " | "
            --winners <- (read <$> (spaces >> many1 digit)) `sepBy`string " "
            pure (i, trace (show hand) $ parseNs . trim $ hand!!0, parseNs . trim $ hand!!1)

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
