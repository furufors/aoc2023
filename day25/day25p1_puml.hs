#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List.Split
import qualified Data.Set as S

main :: IO ()
main = interact $ plantUml . concatMap parse . lines

plantUml :: [(String, String)] -> String
plantUml ss = let pairs = map toGraph ss
                  elems = foldl collect S.empty ss
                  nodes = map toNode $ S.toList elems
                  headr = "@startuml\n"
                  footr = "@enduml\n"
                  collect :: S.Set String -> (String,String) -> S.Set String
                  collect s (a,b) = S.insert a $ S.insert b s
              in headr ++ unlines pairs ++ unlines nodes ++ footr

toNode :: String -> String
toNode s = s ++ ": " ++ s

toGraph :: (String, String) -> String
toGraph (a,b) = a ++ " --> " ++ b

parse :: String -> [(String, String)]
parse s = let (a,b) = span (/=':') s
              bs = splitOn " " $ drop 2 b
          in map (\e -> (a,e)) bs
