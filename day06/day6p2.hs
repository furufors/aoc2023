#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Either
import Data.List.Split (splitOn)
import Text.Parsec
import Text.Parsec.Char

main :: IO ()
main = interact $ show . game . parseline

game :: (Int,Int) -> Int
game (t, d) = sum [ 1 | w <- [0..(t)], win (t-w) d w ]

win :: Int -> Int -> Int -> Bool
win t d s = s * t > d

parseline :: String -> (Int, Int)
parseline = fromRight (error "parse fail") . parse round ""
    where
        round = do
            time <- string "Time:" >> spaces >> (spaces >> many1 digit) `sepBy` string " "
            string "\n"
            dist <- string "Distance:" >> spaces >> (spaces >> many1 digit) `sepBy` string " "
            pure $ (read . concat $ time, read . concat $ dist)
