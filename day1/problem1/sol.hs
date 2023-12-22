module Main where
import Data.Char (isDigit, digitToInt)

sol :: [String] -> Int
sol ls = foldr (\line acc -> acc + getNumFromLine line) 0 ls
    where
        getNumFromLine :: String -> Int
        getNumFromLine s = getFirstDigit s*10 + getFirstDigit (reverse s)
        getFirstDigit :: String -> Int
        getFirstDigit [] = 0
        getFirstDigit (c:cs) = if isDigit c then digitToInt c else getFirstDigit cs

main :: IO ()
main = do
    inputLines <- readFile "input.txt"
    print $ sol (lines inputLines)