module Main where
import Data.Foldable (foldl')
import Data.Char
import Data.List (intersect)

strSplit :: (Char -> Bool) -> String -> [String]
strSplit f s = go [] "" f s
    where
        go :: [String] -> String -> (Char -> Bool) -> String -> [String]
        go res acc _ [] = res ++ [acc]
        go res acc f (c:cs) = if f c then go (res ++ [acc]) "" f cs else go res (acc ++ [c]) f cs


solveAll :: [Int] -> [Int] -> Int
solveAll times dists = product $ zipWith (curry solve) times dists

solve :: (Int, Int) -> Int
solve (time, dist) = go 0 0
    where
        go :: Int -> Int -> Int
        go holdTime numWays
            | holdTime > time = numWays
            | otherwise = go (holdTime + 1) (numWays + if holdTime*(time-holdTime) > dist then 1 else 0)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let times :: [Int] = map read $ strSplit (==' ') $ head $ lines input
    let dists :: [Int] = map read $ strSplit (==' ') $ head $ tail $ lines input
    print $ solveAll times dists
