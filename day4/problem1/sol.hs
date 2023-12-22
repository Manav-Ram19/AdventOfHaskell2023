module Main where
import Data.Foldable (foldl')
import Data.Char
import Data.List (intersect)

-- >>> resolveLine "Card   1: 33 56 23 64 92 86 94  7 59 13 | 86 92 64 43 10 70 16 55 79 33 56  8  7 25 82 14 31 96 94 13 99 29 69 75 23"
-- (" 33 56 23 64 92 86 94  7 59 13 "," 86 92 64 43 10 70 16 55 79 33 56  8  7 25 82 14 31 96 94 13 99 29 69 75 23")

-- >>> numStrToNums " 33 56 23 64 92 86 94  7 59 13 "
-- [33,56,23,64,92,86,94,7,59,13]

-- >>> numStrToNums " 86 92 64 43 10 70 16 55 79 33 56  8  7 25 82 14 31 96 94 13 99 29 69 75 23"
-- [86,92,64,43,10,70,16,55,79,33,56,8,7,25,82,14,31,96,94,13,99,29,69,75,23]

numStrToNums :: String -> [Int]
numStrToNums = go 0 []
    where
        go :: Int -> [Int] -> String -> [Int]
        go acc res [] =
            if acc == 0
                then res
                else res ++ [acc]
        go acc res (c:cs)
          | isDigit c = go (acc*10 + digitToInt c) res cs
          | acc == 0 = go acc res cs
          | otherwise = go 0 (res ++ [acc]) cs

resolveLine :: String -> Int
resolveLine line = if numMatches == 0 then 0 else 2^(numMatches-1)
    where
        numMatches = length (actualCardNums `intersect` targetCardNums)
        actualCardNums = numStrToNums actualCardNumsAsStrs
        targetCardNums = numStrToNums targetCardNumsAsStrs
        (targetCardNumsAsStrs, actualCardNumsAsStrs) = toTwoTuple $ strSplit (== '|' ) $ head $ removeCardNumber line
        removeCardNumber l = tail $ strSplit (== ':') l
        toTwoTuple :: [a] -> (a,a)
        toTwoTuple [a,b] = (a,b)
        toTwoTuple _ = error "invalid toTwoTuple case"

strSplit :: (Char -> Bool) -> String -> [String]
strSplit f s = go [] "" f s
    where
        go :: [String] -> String -> (Char -> Bool) -> String -> [String]
        go res acc _ [] = res ++ [acc]
        go res acc f (c:cs) = if f c then go (res ++ [acc]) "" f cs else go res (acc ++ [c]) f cs

strSlice :: (Char -> Bool) -> String -> String
strSlice f s = go "" f s
    where
        go :: String -> (Char -> Bool) -> String -> String
        go acc _ [] = acc
        go acc f (c:cs) = if f c then acc else go (acc ++ [c]) f cs


sol :: [String] -> Int
sol s = sum $ map resolveLine s

main :: IO ()
main = do
    inputLines <- readFile "input.txt"
    print $ sol (lines inputLines)
