#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

main :: IO ()
main = interact $ show . sum . map fst . filter symbolAdjacent . toAdjacents . lines

toAdjacents :: [[Char]] -> [(Int, [Char])]
toAdjacents css = toA 0 0 []
    where
        h = length css
        w = length (head css)
        toA :: Int -> Int -> [(Int, [Char])] -> [(Int, [Char])]
        toA x y acc | x + 1 >= w && y + 1 >= h = acc
        toA x y acc | x + 1 >= w = toA 0 (y + 1) acc
        toA x y acc = if css!!y!!x `elem` ['0'..'9']
                      then let str = drop x (css!!y)
                               i = takeWhile (`elem` ['0'..'9']) str
                               l = length i
                               ns = [ css!!y!!x | y <- [y-1, y, y+1], x <-[(x-1)..(x+l)], x >= 0, x < w, y >= 0, y < h]
                           in toA (x+l) y ((read i, ns):acc)
                      else toA (x+1) y acc

symbolAdjacent :: (Int, [Char]) -> Bool
symbolAdjacent (i, cs) = (>0) . length $ filter (\c -> not $ c `elem` ('.':['0'..'9'])) cs
