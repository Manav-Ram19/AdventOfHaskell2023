module Main where
import Data.Foldable (foldl')
import Data.Char
import Data.List (intersect, sort)
import Data.Map

strSplit :: (Char -> Bool) -> String -> [String]
strSplit f s = go [] "" f s
    where
        go :: [String] -> String -> (Char -> Bool) -> String -> [String]
        go res acc _ [] = res ++ [acc]
        go res acc f (c:cs) = if f c then go (res ++ [acc]) "" f cs else go res (acc ++ [c]) f cs

solve :: [String] -> Int
solve lines = sum bidWithRank
    where
        bidWithRank = Prelude.map (\(r, MkHand _ _ b) -> r*b) handsWithRanks
        handsWithRanks = zip [1..(length hands)] (sort hands)
        hands = Prelude.map parseLineIntoHand lines

type HandCards = (Int, Int, Int, Int, Int)
type Bid = Int
data HandType = HIGH | ONE | TWO | THREE | FULL | FOUR | FIVE deriving (Eq, Ord, Show)

data Hand = MkHand HandType HandCards Bid deriving (Eq, Ord, Show)

parseLineIntoHand :: String -> Hand
parseLineIntoHand line = MkHand (handCardsToHandType handCards) handCards bid
    where
        bid = read (head $ tail handAndBidStr) :: Int
        handCards = to5Tuple $ Prelude.map cardCharToInt $ head handAndBidStr
        handAndBidStr = strSplit (==' ') line
        cardCharToInt :: Char -> Int
        cardCharToInt c
            | isDigit c = digitToInt c
            | c == 'T' = 10
            | c == 'J' = 0
            | c == 'Q' = 12
            | c == 'K' = 13
            | c == 'A' = 14
            | otherwise = error "Invalid Card"
        to5Tuple :: [a] -> (a,a,a,a,a)
        to5Tuple l
            | length l == 5 = (head l, l !! 1, l !! 2, l !! 3, l !! 4)
            | otherwise = error "Invalid to5Tuple input"


handCardsToHandType :: HandCards -> HandType
handCardsToHandType hc = process $ go hc
    where
        process :: Map Int Int -> HandType
        process m
            | not (Data.Map.null (Data.Map.filterWithKey (\k _ -> k == 0) m)) = process (addToMaxVal (delete 0 m) (Data.Map.findWithDefault 0 0 m))
            | length m <= 1 = FIVE
            | length m == 2 = if length (Data.Map.filter (== 4) m) == 1 then FOUR else FULL
            | length m == 3 = if length (Data.Map.filter (== 3) m) == 1 then THREE else TWO
            | length m == 4 = ONE
            | length m == 5 = HIGH
        go :: HandCards -> Map Int Int
        go (a,b,c,d,e) =  Data.Foldable.foldl' (\acc v -> insertWith (+) v 1 acc) empty [a,b,c,d,e]

addToMaxVal :: Map Int Int -> Int -> Map Int Int
addToMaxVal m i 
    | Data.Map.null m = empty
    | otherwise = insertWith (+) maxValKey i m
    where
        maxValKey = foldrWithKey (\k v acc -> if findWithDefault 0 acc m < v then k else acc) 0 m

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ solve $ lines input
