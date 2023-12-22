module Main where
import Data.Char (isDigit, digitToInt, intToDigit)
import Data.Foldable (Foldable(foldl'))

sol :: [String] -> Int
sol ls = foldr (\line acc -> acc + getNumFromLine line) 0 ls
    where
        getNumFromLine :: String -> Int
        getNumFromLine s = getFirstDigit s*10 + getLastDigit (getFirstDigit s) s
        getFirstDigit :: String -> Int
        getFirstDigit [] = 0
        getFirstDigit ccs@(c:cs) =
            if isDigit c
                then digitToInt c
                else case findMatchingNumPrefix ccs of
                    Just n -> n
                    Nothing -> getFirstDigit cs
        getLastDigit :: Int -> String -> Int
        getLastDigit def [] = def
        getLastDigit def s@(c:cs) = 
            case findMatchingNumPrefix s of
                Just n -> getLastDigit n cs
                Nothing -> getLastDigit def cs
        findMatchingNumPrefix :: String -> Maybe Int
        findMatchingNumPrefix s = foldl' (flip (go s)) Nothing allStrsToVals
            where
                go :: String -> (String, Int) -> Maybe Int -> Maybe Int
                go _ _ (Just n) = Just n
                go [] _ Nothing = Nothing
                go s (ns, n) _ = if isMatchingPrefix s ns then Just n else Nothing
        isMatchingPrefix :: String -> String -> Bool
        isMatchingPrefix _ [] = True
        isMatchingPrefix [] _ = False
        isMatchingPrefix (s:ss) (c:cs) = (s == c) && isMatchingPrefix ss cs
        digitToNum :: [(String, Int)]
        digitToNum = map (\c -> (show c, c)) [0..9]
        strToNum :: [(String, Int)]
        strToNum = [
            ("zero" , 0),
            ("one" , 1),
            ("two" , 2),
            ("three" , 3),
            ("four" , 4),
            ("five" , 5),
            ("six" , 6),
            ("seven" , 7),
            ("eight" , 8),
            ("nine" , 9)]
        allStrsToVals :: [(String, Int)]
        allStrsToVals = digitToNum ++ strToNum

main :: IO ()
main = do
    inputLines <- readFile "input.txt"
    print $ sol (lines inputLines)