#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List.Split (splitOn)
import Data.Maybe
import qualified Data.Map as M
import Data.Either
import Text.Parsec
type RuleMap = M.Map String [InnerRule]
type Rule = (String, [InnerRule])
type Item = (Char, Int)
type InnerRule = Either (Char, Operator, Int, Action) Action
data Action = Next String | Reject | Accept deriving (Show, Eq)
data Operator = Less | More deriving (Show, Eq)

main :: IO ()
main = interact $ show . run . parseIn . splitOn "\n\n"

run :: (RuleMap, [[Item]]) -> Int
run (rs, is) = sum [ sum (map snd i) | i <- is, isAccepted "in" i ]
    where
        isAccepted lbl item = case M.lookup lbl rs of
            Just rules -> let nextLbl = runRules rules item
                          in case nextLbl of
                            Next str -> isAccepted str item
                            Reject -> False
                            Accept -> True
            Nothing -> error $ "Missing label " ++ lbl
        runRules :: [InnerRule] -> [Item] -> Action
        runRules [] is = error "Reached end of rules"
        runRules ((Left (c, op, int, act)):rs) is = case op of
            Less -> if getElem c is < int then act else runRules rs is
            More -> if getElem c is > int then act else runRules rs is
        runRules ((Right act):[]) _ = act
        runRules ((Right act):rs) _ = error "Action not at end"
        getElem :: Char -> [(Char, Int)] -> Int
        getElem c [] = error $ "Missing property " ++ [c]
        getElem c ((c1,int):is) = if c == c1 then int else getElem c is


parseIn :: [String] -> (RuleMap,[[Item]])
parseIn as = let rules = map parseRules . lines $ head as
                 items = map parseItems . lines $ last as
         in (M.fromList rules, items)

parseItems :: String -> [Item]
parseItems input = case parse (try parseItem) "parsein" input of
    Left err -> error $ show err
    Right a -> a

parseItem :: Parsec String () [Item]
parseItem = do
    string "{"
    props <- prop `sepBy` string ","
    string "}"
    pure props

prop :: Parsec String () Item
prop = do
    n <- lower
    string "="
    i <- read <$> many1 digit
    pure (n,i)

parseRules :: String -> Rule
parseRules input = case parse (try parseRule) "parsein" input of
    Left err -> error $ show err
    Right a -> a

parseRule :: Parsec String () Rule
parseRule = do
    name <- many1 lower
    string "{"
    rules <- (try innerRule <|> try innerAction) `sepBy` string ","
    string "}"
    pure (name, rules)

innerRule :: Parsec String () InnerRule
innerRule = do
    prop <- letter
    oper <- operator
    n <- read <$> many1 digit
    string ":"
    name <- action
    pure $ Left (prop, oper, n, name)

innerAction :: Parsec String () InnerRule
innerAction = do
    name <- action
    pure $ Right name

operator :: Parsec String () Operator
operator = (try parseLess <|> try parseMore)
    where
        parseLess = string "<" >> pure Less
        parseMore = string ">" >> pure More

action :: Parsec String () Action
action = (try parseNext <|> try parseAccept <|> try parseReject)
    where
        parseAccept = string "A" >> pure Accept
        parseReject = string "R" >> pure Reject
        parseNext = do
            name <- many1 lower
            pure (Next name)
