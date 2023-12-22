module Main where

strSplit :: (Char -> Bool) -> String -> [String]
strSplit f s = go [] "" f s
    where
        go :: [String] -> String -> (Char -> Bool) -> String -> [String]
        go res acc _ [] = res ++ [acc]
        go res acc f (c:cs) = if f c then go (res ++ [acc]) "" f cs else go res (acc ++ [c]) f cs


solveAll :: [Int] -> [Int] -> Int
solveAll times dists = product $ zipWith (curry solve) times dists

-- >>> solve (71530, 940200)
-- 71503

solve :: (Int, Int) -> Int
solve (time, dist) = largestSpeedThatBeatsDist - smallestSpeedThatBeatsDist + 1
    where
        smallestSpeedThatBeatsDist = lowerSolve 0 mid mid
        largestSpeedThatBeatsDist = upperSolve (mid+1) time mid
        mid = time `div` 2
        lowerSolve :: Int -> Int -> Int -> Int
        lowerSolve l r acc
            | l > r = acc
            | calcDist (getMid l r) > dist = lowerSolve l (getMid l r - 1) (min acc (getMid l r))
            | otherwise = lowerSolve (getMid l r + 1) r acc
        upperSolve :: Int -> Int -> Int -> Int
        upperSolve l r acc
            | l > r = acc
            | calcDist (getMid l r) > dist = upperSolve (getMid l r + 1) r (max acc (getMid l r))
            | otherwise = upperSolve l (getMid l r - 1) acc
        calcDist :: Int -> Int
        calcDist holdTime = holdTime*(time - holdTime)
        getMid :: Int -> Int -> Int
        getMid l r = l + ((r-l) `div` 2)

{- 
holdTime*time - holdTime^2
time - 2*holdTime = 0
holdTime = time / 2
 -}

main :: IO ()
main = do
    input <- readFile "input.txt"
    let time :: Int = read $ head $ lines input
    let dist :: Int = read $ head $ tail $ lines input
    print $ solve (time, dist)
