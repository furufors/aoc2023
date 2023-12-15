#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Char
import Data.List.Split

main :: IO ()
main = interact $ show . sum . map (foldl (\i a -> (17 * (i + ord a)) `rem` 256) 0) . splitOn "," . head . lines
