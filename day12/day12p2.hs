#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Either
import Data.List
import Text.Parsec
import Text.Parsec.Char
import qualified Data.Map as M
data Rec = Op | Dmg | Ukwn deriving (Show, Eq)

main :: IO ()
main = interact $ show . sum . map (countCombs . parseline) . lines

countCombs :: ([Rec], [Int]) -> Int
countCombs (rs,is) = combs rs is
    where
        lrs = length rs
        lis = length is
        isValid = M.fromList [((r,i), combs (drop r rs) (drop i is)) | r <- [0..lrs], i <- [0.. lis]]

        combs :: [Rec] -> [Int] -> Int
        combs [] [] = 1
        combs [] _ = 0
        combs (Dmg:_) [] = 0
        combs (Op:rs) is = combs (dropWhile (==Op) rs) is
        combs (Dmg:rs) (i:is) =
            if length rs >= (i - 1 + sum is) &&
               ((length rs == (i - 1 + sum is)) || (not . (==Dmg) . head . drop (i-1)) rs) &&
               all (/= Op) (take (i-1) rs)
            then combs (drop (i) rs) is
            else 0
        combs (Ukwn:rs) is =
            let key = (lrs - length rs, lis - length is)
            in case M.lookup key isValid of
                Just n -> n + combs (Dmg:rs) is
                Nothing -> error $ "Missing lookup for " ++ show key

parseline :: String -> ([Rec], [Int])
parseline = fromRight (error "parse fail") . parse round ""
    where
        round = do
            rs <- many1 parseRec
            spaces
            is <- (read <$> many1 digit) `sepBy` string ","
            let dmgGroups = intercalate [Ukwn] . take 5 $ repeat rs
            return (dmgGroups, concat . take 5 $ repeat is)
        parseRec = try op <|> try dmg <|> try ukwn
        op   = string "." >> return Op
        dmg  = string "#" >> return Dmg
        ukwn = string "?" >> return Ukwn
