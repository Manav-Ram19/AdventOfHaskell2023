module Main where
import Data.Foldable (foldl')

splitLineByComma :: String -> [String]
splitLineByComma l = strSplit (== ',') l
splitLineBySemiColon :: String -> [String]
splitLineBySemiColon l = strSplit (== ';') l

resolveLine :: String -> Int
resolveLine line =
    let gameIdAndDetails = strSplit (== ':') line in
        getFinalScore gameIdAndDetails
        where
            getGameId s = read (drop (length "Game ") s) :: Int
            getFinalScore :: [String] -> Int
            getFinalScore [] = error "Invalid Call"
            getFinalScore [gameId, gameDetails] = 
                if isValidGame gameDetails then getGameId gameId else 0
            isValidGame :: String -> Bool
            isValidGame gameDetails =
                let everyCubePull = map splitLineByComma $ splitLineBySemiColon gameDetails in
                    foldl' (\acc c -> acc && validateCubePull c) True (concat everyCubePull)
                    where
                        validateCubePull :: String -> Bool
                        validateCubePull s = 
                            let dummyCubeNumAndColor = strSplit (== ' ') s in
                                case head $ tail $ tail dummyCubeNumAndColor of
                                    "red" -> (read (head $ tail dummyCubeNumAndColor) :: Int) <= 12
                                    "blue" -> (read (head $ tail dummyCubeNumAndColor) :: Int) <= 14
                                    "green" -> (read (head $ tail dummyCubeNumAndColor) :: Int) <= 13



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