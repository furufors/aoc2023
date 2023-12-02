#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Either
import Text.Parsec
import Text.Parsec.Char

main :: IO ()
main = interact $ show . sum . map (toPower (0,0,0) . snd . parseline) . lines

toPower :: (Int,Int,Int) -> [(Int,Int,Int)] -> Int
toPower (r,g,b) [] = r*g*b
toPower (r,g,b) ((r1,g1,b1):rs) = toPower (max r r1, max g g1, max b b1) rs

parseline :: String -> (Int, [(Int,Int,Int)])
parseline s = let (i, ps) = fromRight (error "parse fail") . parse round "" $ s
    in (i, map (foldl (\x f -> f x) (0,0,0)) ps)
    where
        round = do
            _ <- string "Game "
            i <- read <$> many1 digit
            _ <- string ": "
            counts <- (colour `sepBy` string ", ") `sepBy` (string "; ")
            return (i, counts)
        colour = try red <|> try green <|> try blue
        red = do
            i <- read <$> many1 digit
            _ <- string " red"
            return $ \(r,g,b) -> (r+i, g, b)
        green = do
            i <- read <$> many1 digit
            _ <- string " green"
            return $ \(r,g,b) -> (r, g+i, b)
        blue = do
            i <- read <$> many1 digit
            _ <- string " blue"
            return $ \(r,g,b) -> (r, g, b+i)
