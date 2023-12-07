#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List
import Data.Ord
type Card = Char
type Hand = [Card]
data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Eq, Ord)

main :: IO ()
main = interact $ show . score . sortBy (\(a,b) (c,d) -> cmpCards a c) . map parseIn . lines

parseIn :: String -> (Hand, Int)
parseIn  s =
    let hand = take 5 s
        bet = read $ drop 6 s
    in (hand, bet)

score :: [(Hand, Int)] -> Int
score hs = sum $ zipWith (*) (map snd hs) [1..]

cmpCards :: Hand -> Hand -> Ordering
cmpCards a b = case compare (toCategory a) (toCategory b) of
    GT -> GT
    LT -> LT
    EQ -> a `higherCardsThan`b

toCategory :: Hand -> HandType
toCategory c
    | isFiveOfAkind  = FiveOfAKind
    | isFourOfAKind  = FourOfAKind
    | isFullHouse    = FullHouse
    | isThreeOfAkind = ThreeOfAKind
    | isTwoPair      = TwoPair
    | isOnePair      = OnePair
    | otherwise      = HighCard
    where
        grouped = map length . group . sort $ c
        isFiveOfAkind  = 5 `elem` grouped
        isFourOfAKind  = 4 `elem` grouped
        isFullHouse    = 3 `elem` grouped && 2 `elem` grouped
        isThreeOfAkind = 3 `elem` grouped
        isTwoPair      = (==2) . length $ filter (==2) grouped
        isOnePair      = 2 `elem` grouped

higherCardsThan :: Hand -> Hand -> Ordering
higherCardsThan [    ] ______ = EQ
higherCardsThan ______ [    ] = EQ
higherCardsThan (a:as) (b:bs) =
    if a == b
    then as `higherCardsThan` bs
    else if (cardStrength a) > (cardStrength b)
         then GT
         else LT

cardStrength :: Card -> Int
cardStrength c =
    let ranking = reverse ['A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2']
        index = case elemIndex c ranking of
            Just a -> a
            otherwise -> error $ "Unknown card " ++ [c]
    in index
