#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Char
import Data.List.Split
import qualified Data.Map as M
data Action = Add | Remove
type Lens = (String, Int)
type Boxes = M.Map Int [Lens]

main :: IO ()
main = interact $ show . score . foldl lens M.empty . splitOn "," . head . lines

score :: Boxes -> Int
score bs = sum $ map score' (M.assocs bs)

score' :: (Int, [Lens]) -> Int
score' (bn,ls) = sum . map ((bn + 1)*) $ zipWith (*) (map snd ls) [1..]

lens :: Boxes -> String -> Boxes
lens bs str = let lensName = takeWhile (\c -> not $ c `elem` "=-") str
                  boxNr = hashStr lensName
                  action = head $ dropWhile (\c -> not $ c `elem` "=-") str
              in case action of
                '=' -> let i = read (tail $ dropWhile (\c -> not $ c `elem` "=-") str)
                           e = (lensName, i)
                       in case M.lookup boxNr bs of
                            Nothing -> M.insert boxNr [e] bs
                            Just es -> M.insert boxNr (insert e es) bs
                '-' -> case M.lookup boxNr bs of
                        Nothing -> bs
                        Just es -> M.insert boxNr (remove lensName es) bs

remove :: String -> [Lens] -> [Lens]
remove ln [] = []
remove ln ((an,ai):as) = if ln == an then as else (an,ai):(remove ln as)

insert :: Lens -> [Lens] -> [Lens]
insert l [] = [l]
insert (ln,li) ((an, ai):as) = if ln == an then (ln,li):as else (an,ai):(insert (ln,li) as)

hashStr = foldl (\i a -> (17 * (i + ord a)) `rem` 256) 0
