#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

main :: IO ()
main = interact $ show . sum . map fst . filter symbolAdjacent . toAdjacents . lines

toAdjacents :: [[Char]] -> [(Int, [Char])]
toAdjacents css =
    let height = length css
        width = length (head css)
    in toA width height 0 0 []
    where
        toA :: Int -> Int -> Int -> Int -> [(Int, [Char])] -> [(Int, [Char])]
        toA w h x y acc | x + 1 >= w && y + 1 >= h = acc
        toA w h x y acc | x + 1 >= w = toA w h 0 (y + 1) acc
        toA w h x y acc = if css!!y!!x `elem` ['0'..'9']
                          then let str = drop x (css!!y)
                                   i = takeWhile (`elem` ['0'..'9']) str
                                   l = length i
                                   ns = [ css!!y!!x | y <- [y-1, y, y+1], x <-[(x-1)..(x+l)], x >= 0, x < w, y >= 0, y < h]
                               in toA w h (x+l) y ((read i, ns):acc)
                          else toA w h (x+1) y acc

symbolAdjacent :: (Int, [Char]) -> Bool
symbolAdjacent (i, cs) = let remaining = length $ filter (\c -> not $ c `elem` ('.':['0'..'9'])) cs
                         in remaining > 0
