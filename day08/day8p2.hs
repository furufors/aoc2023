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
main = interact $ show . foldl lcm 1 . run2 . (\ls -> (cycle $ head ls, parselines (lines $ last ls))) . splitOn "\n\n"

run2 :: ([Char], LRMap) -> [Int]
run2 (ds,mp) = let endWithA = filter (\p -> (last p) == 'A') $ M.keys mp
    in map (\p -> run 0 p (ds,mp)) endWithA

run :: Int -> Label -> ([Char], LRMap) -> Int
run i pos _ | (last pos) == 'Z' = i
run i pos (d:ds, mp) | d == 'R' = run (i+1) (snd $ next) (ds, mp)
                     | d == 'L' = run (i+1) (fst $ next) (ds, mp)
    where
        next = case M.lookup pos mp of
            Just a -> a
            Nothing -> error $ "Missing position " ++ pos

parselines :: [String] -> LRMap
parselines = M.fromList . map (fromRight (error "parse fail") . parse round "")
    where
        round = do
            pos <- many1 alphaNum
            left <- string " = (" >> many1 alphaNum
            right <- string ", " >> many1 alphaNum
            string ")"
            pure $ (pos,(left,right))
