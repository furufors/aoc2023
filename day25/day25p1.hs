#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Algorithm.Search
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as M
type Flow = M.Map (String,String) Int
type Verts = M.Map String (S.Set String)

main :: IO ()
main = interact $ show . findMaxFlow . toVerts M.empty . concatMap parse . lines

toVerts :: Verts -> [(String,String)] -> Verts
toVerts vs [] = vs
toVerts vs ((f,t):es) = let vs' = case M.lookup f vs of
                                    Just a -> M.insert f (S.insert t a) vs
                                    Nothing -> M.insert f (S.insert t S.empty) vs
                        in case M.lookup t vs of
                                Just a -> toVerts (M.insert t (S.insert f a) vs') es
                                Nothing -> toVerts (M.insert t (S.insert f S.empty) vs') es

findMaxFlow :: Verts -> Int
findMaxFlow vs =
    let ns = M.keys vs
        findPath a b = snd . fromJust $ dijkstra (\p -> fromJust $ M.lookup p vs) (const $ const 1) (==b) a
        paths = [ zip (a:es) (es++[b]) | (a,b) <- take 5000 $ [(a,b) | a <- ns, b <- ns,  a /= b]
             , let es = findPath a b]
        occur = foldl addVertVis M.empty $ concat paths
        sorting a b = compare (snd a) (snd b)
        priority = take 10 $ map fst . reverse . sortBy sorting $ M.assocs occur
        (a,b,c) = head [(a,b,c) | a <- priority, b <- priority, c <- priority
                       , a /= b && b /= c && c /= a
                       , let vs' = remove a $ remove b $ remove c $ vs
                       , any null . take 20 $ [findPath' vs' f t | f <- M.keys vs', t <- M.keys vs', f /= t]
                       ]
        findPath' verts from to = bfs (\f -> fromJust $ M.lookup f verts) (==to) from
        correctSplit = remove a $ remove b $ remove c $ vs
        startPoint = head $ M.keys correctSplit
        sizeA = S.size $ extent correctSplit (S.empty) startPoint
        totalSize = length $ M.keys correctSplit
    in sizeA * (totalSize - sizeA)

addVertVis :: Flow -> (String, String) -> Flow
addVertVis f (a,b) = let k = (min a b, max a b)
                  in case M.lookup k f of
                        Just i  -> M.insert k (i + 1) f
                        Nothing -> M.insert k (1) f

remove :: (String,String) -> Verts -> Verts
remove (a,b) verts = let verts' = case M.lookup a verts of
                                    Just r -> M.insert a (S.delete b r) verts
                                    Nothing -> verts
                     in case M.lookup b verts' of
                            Just r -> M.insert b (S.delete a r) verts'
                            Nothing -> verts'

extent :: Verts -> (S.Set String) -> String -> (S.Set String)
extent vs vis s = if S.member s vis
                  then vis
                  else case M.lookup s vs of
                        Just ns -> foldl (extent vs) (S.insert s vis) ns
                        Nothing -> vis

parse :: String -> [(String, String)]
parse s = let (a,b) = span (/=':') s
              bs = splitOn " " $ drop 2 b
          in map (\e -> (a,e)) bs
