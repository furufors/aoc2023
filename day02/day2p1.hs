#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Either
import Text.Parsec
import Text.Parsec.Char

main :: IO ()
main = interact $ show . sum . map fst . filter (constraint (12,13,14)) .  map parseline . lines

constraint :: (Int,Int,Int) -> (Int, [(Int,Int,Int)]) -> Bool
constraint _ (i,[]) = True
constraint (r,g,b) (i,((r1,g1,b1):rs)) = and [r1 <= r, g1 <= g, b1 <= b, constraint (r,g,b) (i,rs)]

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
