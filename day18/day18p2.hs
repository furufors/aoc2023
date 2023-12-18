#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Char (digitToInt, toUpper,ord)

main :: IO ()
main = interact $ show . gaussArea . (\x -> (x, foldl toPolygon [(0,0)] x)) . map parsein . lines

parsein :: String -> (Int,Int)
parsein s = let (a,b) = span (/='#') s
                hex = take 5 $ drop 1 b
                dig = take 1 $ drop 6 b
                n = hexToDecimal hex
            in case dig of
                "0" -> (n, 0)
                "1" -> (0, -1 * n)
                "2" -> (-1 * n, 0)
                "3" -> (0, n)

toPolygon :: [(Int,Int)] -> (Int,Int) -> [(Int,Int)]
toPolygon (a:as) (x,y) = let (px,py) = a in (px + x, py + y):a:as

gaussArea :: ([(Int,Int)],[(Int,Int)]) -> Int
gaussArea (ds,ps) =
    let interior = abs . sum $ zipWith (\(x1, y1) (x2, y2) -> (y1 + y2) * (x2 - x1)) ps (tail ps)
        exterior = sum . map (\(a,b) -> abs a + abs b) $ ds
        pick = (interior - exterior + 2) `div` 2
    in pick + exterior


hexToDecimal :: String -> Int
hexToDecimal = foldl (\acc x -> acc * 16 + hexDigitToInt x) 0 . map toUpper
   where hexDigitToInt x
          | x >= '0' && x <= '9' = ord x - ord '0'
          | x >= 'A' && x <= 'F' = ord x - ord 'A' + 10
