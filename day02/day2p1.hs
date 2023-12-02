#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Text.Parsec
import Text.Parsec.Char

main :: IO ()
main = interact $ show . sum . map fst . filter (constraint (12,13,14)) .  map parseline . lines

constraint :: (Int,Int,Int) -> (Int, [(Int,Int,Int)]) -> Bool
constraint _ (i,[]) = True
constraint (r,g,b) (i,((r1,g1,b1):rs)) = and [r1 <= r, g1 <= g, b1 <= b, constraint (r,g,b) (i,rs)]

parseline :: String -> (Int, [(Int,Int,Int)])
parseline s = let (i, ps) = fromRight (error "parse fail") . parse numbers "" $ s
    in (i, map combine ps)
    where
        numbers = do
            _ <- string "Game "
            i <- read <$> many1 digit
            _ <- string ": "
            counts <- (colour `sepBy` string ", ") `sepBy` (string "; ")
            return (i, counts)
        colour = try red <|> try green <|> try blue
        red = do
            i <- read <$> many1 digit
            _ <- string " red"
            return (i, 0, 0)
        green = do
            i <- read <$> many1 digit
            _ <- string " green"
            return (0, i, 0)
        blue = do
            i <- read <$> many1 digit
            _ <- string " blue"
            return (0, 0, i)

combine :: [(Int,Int,Int)] -> (Int,Int,Int)
combine [] = (0,0,0)
combine ((r,g,b):rs) = let (r1, g1, b1) = combine rs
                       in (r+r1, g+g1, b+b1)
