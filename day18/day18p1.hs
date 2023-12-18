#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
main :: IO ()
main = interact $ show . gaussArea . (\x -> (x, foldl toPolygon [(0,0)] x)) . map parsein . lines

parsein :: String -> (Int,Int)
parsein s = let (a,b) = span (/=' ') s
                (c,d) = span (/=' ') $ tail b
                n = read c
            in case a of
                "R" -> (n, 0)
                "D" -> (0, -1 * n)
                "L" -> (-1 * n, 0)
                "U" -> (0, n)

toPolygon :: [(Int,Int)] -> (Int,Int) -> [(Int,Int)]
toPolygon (a:as) (x,y) = let (px,py) = a in (px + x, py + y):a:as

gaussArea :: ([(Int,Int)],[(Int,Int)]) -> Int
gaussArea (ds,ps) =
    let interior = abs . sum $ zipWith (\(x1, y1) (x2, y2) -> (y1 + y2) * (x2 - x1)) ps (tail ps)
        exterior = sum . map (\(a,b) -> abs a + abs b) $ ds
        pick = (interior - exterior + 2) `div` 2
    in pick + exterior
