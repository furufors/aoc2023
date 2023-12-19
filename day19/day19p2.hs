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
type Bounds = (Char, Int, Int)
type InnerRule = Either (Char, Operator, Int, Action) Action
data Action = Next String | Reject | Accept deriving (Show, Eq)
data Operator = Less | More deriving (Show, Eq)

main :: IO ()
main = interact $ show . run . parseIn . splitOn "\n\n"

calculate :: [[Bounds]] -> Int
calculate bs = sum $ map (combs 1) bs

combs :: Int -> [Bounds] -> Int
combs i [] = i
combs i ((_,l,h):bs) = if l <= h then combs ((h - l + 1) * i) bs else 0

run :: (RuleMap, [[Item]]) -> Int
run (rs, is) = calculate $ acceptedBounds (Next "in",[('x',1, 4000),('m',1, 4000),('a',1, 4000),('s',1, 4000)])
    where
        acceptedBounds :: (Action, [Bounds]) -> [[Bounds]]
        acceptedBounds (Reject, bounds) = []
        acceptedBounds (Accept, bounds) = [bounds]
        acceptedBounds (Next lbl, bounds) = case M.lookup lbl rs of
            Just rules -> concatMap acceptedBounds $ runRules rules bounds
            Nothing -> error $ "Missing label " ++ lbl

        runRules :: [InnerRule] -> [Bounds] -> [(Action, [Bounds])]
        runRules [] _ = []
        runRules ((Right act):rs) bounds = [(act,bounds)]
        runRules ((Left (c,o,i,a)):rs) bounds = case o of
            Less -> [(a, replaceHigh c (i-1) bounds)] ++ runRules rs (replaceLow  c i bounds)
            More -> [(a, replaceLow  c (i+1) bounds)] ++ runRules rs (replaceHigh c i bounds)
            where
                replaceLow :: Char -> Int -> [Bounds] -> [Bounds]
                replaceLow c i [] = []
                replaceLow c i ((c1,l,h):as) = if c == c1 then (c, max l i,h):as else (c1,l,h):(replaceLow c i as)

                replaceHigh :: Char -> Int -> [Bounds] -> [Bounds]
                replaceHigh c i [] = []
                replaceHigh c i ((c1,l,h):as) = if c == c1 then (c,l, min h i):as else (c1,l,h):(replaceHigh c i as)

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
