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
main = interact $ show . findMaxFlow . foldl toVerts M.empty . concatMap parse . lines

toVerts :: Verts -> (String,String) -> Verts
toVerts vs (f,t) = let fv = fromMaybe S.empty $ M.lookup f vs
                       tv = fromMaybe S.empty $ M.lookup t vs
                   in M.insert f (S.insert t fv) $ M.insert t (S.insert f tv) vs

findMaxFlow :: Verts -> Int
findMaxFlow vs =
    let findPath a b = snd . fromJust $ dijkstra (\p -> fromJust $ M.lookup p vs) (const $ const 1) (==b) a
        paths = [ zip (a:es) (es++[b]) | (a,b) <- take 5000 $ [(a,b) | a <- M.keys vs, b <- M.keys vs, a /= b]
                , let es = findPath a b]
        occur = foldl addVertVis M.empty $ concat paths
        sorting a b = compare (snd a) (snd b)
        priority = take 10 $ map fst . reverse . sortBy sorting $ M.assocs occur
        hit = head [ [a,b,c] | a <- priority, b <- priority, c <- priority
                   , a /= b && b /= c && c /= a
                   , let vs' = foldl remove vs [a,b,c]
                   , any null . take 20 $ [findPath' vs' f t | f <- M.keys vs', t <- M.keys vs', f /= t]
                   ]
        findPath' verts from to = bfs (\f -> fromJust $ M.lookup f verts) (==to) from
        correctSplit = foldl remove vs hit
        sizeA = S.size $ extent correctSplit (S.empty) (head $ M.keys correctSplit)
        totalSize = length $ M.keys correctSplit
    in sizeA * (totalSize - sizeA)

addVertVis :: Flow -> (String, String) -> Flow
addVertVis f (a,b) = let k = (min a b, max a b)
                  in fromMaybe (M.insert k 1 f) ((\i -> M.insert k (i+1) f) <$> M.lookup k f)

remove :: Verts -> (String,String) -> Verts
remove verts (a,b) = let ar = fromMaybe verts ((\r -> M.insert a (S.delete b r) verts) <$> M.lookup a verts)
                     in fromMaybe ar ((\r -> M.insert b (S.delete a r) ar) <$> M.lookup b ar)

extent :: Verts -> (S.Set String) -> String -> (S.Set String)
extent vs vis s | S.member s vis = vis
extent vs vis s = fromMaybe vis (foldl (extent vs) (S.insert s vis) <$> M.lookup s vs)

parse :: String -> [(String, String)]
parse s = let (a,b) = span (/=':') s
              bs = splitOn " " $ drop 2 b
          in map (\e -> (a,e)) bs
