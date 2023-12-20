#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace
import Data.Either
import Text.Parsec
data RuleType = BroadCaster | FlipFlop | Conjunction deriving (Eq,Ord,Show)
type FlipFlopState = M.Map String Bool
type SubState = M.Map String Bool
type ConjunctionState = M.Map String SubState
type Mappings = M.Map String (RuleType, [String])
type Rule = (RuleType, String, [String])

main :: IO ()
main = interact $ show . run . buildMaps . map parseRules . lines

run :: (Mappings,ConjunctionState) -> Int
run (mp,cs1) =
    let (rl,rh) = run1 1000 (0,0) M.empty cs1 [("broadcaster", False, "button")]
    in rl * rh
    where
        run1 :: Int -> (Int,Int) -> FlipFlopState -> ConjunctionState -> [(String,Bool,String)] -> (Int,Int)
        run1 0 (l,h) _ _ _ = (l,h)
        run1 i (l,h) fs cs [] = run1 (i-1) (l,h) fs cs [("broadcaster", False, "button")]
        run1 i (l,h) fs cs ((str,sig,from):as) =
            case M.lookup str mp of
                Just (t,targets) -> case t of
                    BroadCaster -> if sig
                                   then run1 i (l,h+1) fs cs (as ++ [(tr,False,str) | tr <- targets])
                                   else run1 i (l+1,h) fs cs (as ++ [(tr,False,str) | tr <- targets])
                    FlipFlop -> if sig
                                then run1 i (l,h+1) fs cs as
                                else let state = getFlipFlop str fs
                                         fs' = M.insert str (not state) fs
                                         score = (l+1,h)
                                     in if state
                                        then run1 i score fs' cs (as ++ [(tr,False,str) | tr <- targets])
                                        else run1 i score fs' cs (as ++ [(tr,True,str) | tr <- targets])

                    Conjunction -> let cs' = setConjunction str from sig cs
                                       score = if sig then (l,h+1) else (l+1,h)
                                   in if all id (getConjunction str cs')
                                      then run1 i score fs cs' (as ++ [(tr,False,str) | tr <- targets])
                                      else run1 i score fs cs' (as ++ [(tr,True,str) | tr <- targets])
                Nothing -> if sig
                           then run1 i (l,h+1) fs cs as
                           else run1 i (l+1,h) fs cs as

getConjunction :: String -> ConjunctionState -> [Bool]
getConjunction s cs = case M.lookup s cs of
    Just ss -> M.elems ss
    Nothing -> error $ "Missing SubState map for " ++ s

setConjunction :: String -> String -> Bool -> ConjunctionState -> ConjunctionState
setConjunction s f b cs = case M.lookup s cs of
    Just ss -> M.insert s (M.insert f b ss) cs
    Nothing -> error $ "Missing SubState map for " ++ s

buildMaps :: [Rule] -> (Mappings, ConjunctionState)
buildMaps rs = let mpgs = M.fromList . (++ [("output",(FlipFlop,[]))]) . map (\(t,n,ts) -> (n,(t,ts))) $ rs
                   cjst = foldl addConjunctions M.empty rs
                   addConjunctions :: ConjunctionState -> Rule -> ConjunctionState
                   addConjunctions cs (_,f,tos) = foldl (addCon f) cs tos -- each to shall have map of froms
                   addCon :: String -> ConjunctionState -> String -> ConjunctionState
                   addCon f cs to = case M.lookup to cs of
                    Just a -> M.insert to (M.insert f False a) cs
                    Nothing -> M.insert to (M.insert f False M.empty) cs
               in (mpgs,cjst)

getFlipFlop :: String -> FlipFlopState -> Bool
getFlipFlop str m = case M.lookup str m of
    Just b -> b
    Nothing -> False

parseRules :: String -> Rule
parseRules input = case parse (try parseRule) "parsein" input of
    Left err -> error $ show err
    Right a -> a

parseRule :: Parsec String () Rule
parseRule = do
    name <- many1 (lower <|> oneOf "%&")
    string " -> "
    targets <- (many1 lower) `sepBy` string ", "
    pure $ if head name == '&'
           then (Conjunction, tail name, targets)
           else if name == "broadcaster"
                then (BroadCaster, name, targets)
                else (FlipFlop, tail name, targets)
