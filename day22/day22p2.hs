#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List (delete)
import Data.List.Split
import qualified Data.Map as M
type Coord = (Int,Int,Int) -- x y z
type Block = (Coord,Coord) -- diagonal corners

main :: IO ()
main = interact $ show . countDamage . applyGravity . map parse . lines

countDamage :: [Block] -> Int
countDamage bs = sum [length . filter id $ zipWith (/=) bs' (applyGravity bs') | b <- bs, let bs' = delete b bs]

applyGravity :: [Block] -> [Block]
applyGravity bs = let bs' = gravityStep bs
                  in if bs == bs'
                     then bs
                     else applyGravity bs'
    where
        gravityStep :: [Block] -> [Block]
        gravityStep [    ] = []
        gravityStep (a:as) = let a' = multi a
                                 multi x = if not (atGround x) && all (\b -> b == x || not (overlap (lower x) b)) bs
                                           then multi (lower x)
                                           else x
                             in if a /= a' -- there was a change
                                then a':as -- return on first change
                                else a:(gravityStep as) -- if no changes return original list

lower :: Block -> Block
lower ((x0,y0,z0),(x1,y1,z1)) = ((x0,y0,z0-1),(x1,y1,z1-1))

overlap :: Block -> Block -> Bool
overlap ((x0,y0,z0),(x1,y1,z1)) ((x2,y2,z2),(x3,y3,z3)) =
    overlap1d (x0,x1) (x2,x3) &&
    overlap1d (y0,y1) (y2,y3) &&
    overlap1d (z0,z1) (z2,z3)

overlap1d :: (Int,Int) -> (Int,Int) -> Bool
overlap1d (a,b) (c,d) =
    let l1 = min a b
        h1 = max a b
        l2 = min c d
        h2 = max c d
    in (l1 <= h2) && (l2 <= h1)

atGround :: Block -> Bool
atGround ((x0,y0,z0),(x1,y1,z1)) = z0 <= 1

parse :: String -> Block
parse s = let (a,b) = span (/='~') s
              as = map read $ splitOn "," a
              bs = map read $ splitOn "," $ tail b
          in ((as!!0, as!!1, as!!2), (bs!!0, bs!!1, bs!!2))
