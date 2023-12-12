#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Either
import Text.Parsec
import Text.Parsec.Char
data Rec = Op | Dmg | Ukwn deriving (Show, Eq)

main :: IO ()
main = interact $ show . sum . map (length . filter valid . generate . parseline) . lines

parseline :: String -> ([Rec], [Int])
parseline =fromRight (error "parse fail") . parse round ""
    where
        round = do
            rs <- many1 parseRec
            spaces
            is <- (read <$> many1 digit) `sepBy` string ","
            return (rs, is)
        parseRec = try op <|> try dmg <|> try ukwn
        op   = string "." >> return Op
        dmg  = string "#" >> return Dmg
        ukwn = string "?" >> return Ukwn

generate :: ([Rec],[Int]) -> [([Int],[Rec])]
generate (rs,is) = zip (repeat is) (generate2 rs)

generate2 :: [Rec] -> [[Rec]]
generate2 [] = [[]]
generate2 (Op:rs)  = map (Op:) (generate2 rs)
generate2 (Dmg:rs) = map (Dmg:) (generate2 rs)
generate2 (Ukwn:rs) = map (Op:) (generate2 rs) ++ map (Dmg:)(generate2 rs)

valid :: ([Int],[Rec]) -> Bool
valid (is, rs) = countDmgs [] rs == is

countDmgs :: [Int] -> [Rec] -> [Int]
countDmgs is [] = reverse is
countDmgs is as = case head as of
    Dmg -> let cnt = length $ takeWhile (==Dmg) as
               rest = dropWhile (==Dmg) as
           in countDmgs (cnt:is) rest
    Op -> countDmgs is (dropWhile (==Op) as)
    Ukwn -> error "cannot count unkown"
