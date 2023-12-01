#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

main :: IO ()
main = interact $ show . sum . map (firstLast Nothing Nothing) . lines

firstLast :: Maybe Char -> Maybe Char -> String -> Int
firstLast (Just f) (Just l) [] = read [f, l]
firstLast _        _        [] = error "String didn't contain any number"
firstLast f        l         s = case getNextDigit s of
    Just (d, rest) -> case f of
        Nothing -> firstLast (Just d) (Just d) rest
        Just f  -> firstLast (Just f) (Just d) rest
    Nothing -> firstLast f l (tail s)

getNextDigit :: String -> Maybe (Char, String)
getNextDigit s = if head s `elem` ['1'..'9']
                 then Just (head s, tail s)
                 else textDigit replacements s

textDigit :: [(String, Char)] -> String -> Maybe (Char, String)
textDigit _          [] = Nothing
textDigit []           _= Nothing
textDigit ((n, i):ps) s = if isPrefixOf n s
                          then Just (i, tail s)
                          else textDigit ps s

replacements :: [(String, Char)]
replacements = zip ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] ['1'..'9']

isPrefixOf :: String -> String -> Bool
isPrefixOf as bs = and $ zipWith (==) as (bs ++ repeat '_')
