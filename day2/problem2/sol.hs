module Main where
import Data.Foldable (foldl')

resolveLine :: String -> Int
resolveLine line = getPower $ head $ tail $ strSplit (== ':') line
    where
        getPower :: String -> Int
        getPower l = r*g*b
            where
                (r,g,b) = getMinCubesPerColor l
                getMinCubesPerColor :: String -> (Int, Int, Int)
                getMinCubesPerColor gameDetails =
                    foldl' (\acc round -> pickMaxes acc (getNumCubesFromRound round)) (0,0,0) (strSplit (== ';') gameDetails)
                    where
                        pickMaxes :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
                        pickMaxes (a1,b1,c1) (a2,b2,c2) = (max a1 a2, max b1 b2, max c1 c2)
                        getNumCubesFromRound :: String -> (Int, Int, Int)
                        getNumCubesFromRound round = foldr (combinePicks . getPick) (0,0,0) picks
                            where
                                combinePicks :: (String, Int) -> (Int, Int, Int) -> (Int, Int, Int)
                                combinePicks (color, num) (a,b,c) =
                                    case color of
                                        "red"   -> (a+num, b, c)
                                        "blue"  -> (a, b+num, c)
                                        "green" -> (a, b, c+num)
                                getPick :: String -> (String, Int)
                                getPick s = 
                                    let dummyNumColor = strSplit (== ' ') s in
                                        let color = head $ tail $ tail dummyNumColor in
                                            let num = read (head $ tail dummyNumColor) :: Int in
                                                (color, num)
                                picks = strSplit (== ',') round

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