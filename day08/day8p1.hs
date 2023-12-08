#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Either
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.Char
type Label = String
type LRMap = M.Map Label (Label, Label)

main :: IO ()
main = interact $ show . run 0 "AAA" . (\ls -> (cycle $ head ls, parselines (lines $ last ls))) . splitOn "\n\n"

run :: Int -> Label -> ([Char], LRMap) -> Int
run i "ZZZ" _ = i
run i pos ((dir):ds, mp) = case dir of
    'R' -> run (i+1) (snd $ next) (ds, mp)
    'L' -> run (i+1) (fst $ next) (ds, mp)
    where
        next = case M.lookup pos mp of
            Just a -> a
            Nothing -> error $ "Missing position " ++ pos

parselines :: [String] -> LRMap
parselines = M.fromList . map parseline

parseline :: String -> (Label,(Label, Label))
parseline = fromRight (error "parse fail") . parse round ""
    where
        round = do
            pos <- many1 upper
            string " = ("
            left <- many1 upper
            string ", "
            right <- many1 upper
            string ")"
            pure $ (pos,(left,right))
