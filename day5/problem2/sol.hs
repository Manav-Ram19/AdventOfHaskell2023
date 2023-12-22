module Main where
import Data.Foldable (Foldable(foldl'))
import Data.List (sortBy)


{- 500342114, 59804107 -}
ioSol :: String -> [String] -> IO Int
ioSol inputFile mapFiles = do
    rangeMaps <- collapseIOs $ map getRangeMapFromFile mapFiles
    inputs <- getInputsFromFile inputFile
    pure $ minimum $ map fst $ applyMultipleRangeMapsOnInput inputs rangeMaps

getInputsFromFile :: String -> IO [(Int, Int)]
getInputsFromFile inpFileName = do
    inputLines <- readFile inpFileName
    let inputNums = (map read $ strSplit (==' ') $ head $ lines inputLines):: [Int]
    pure $ collapseInpNumsIntoInpRanges inputNums


collapseInpNumsIntoInpRanges :: [a] -> [(a,a)]
collapseInpNumsIntoInpRanges = go
    where
        go :: [a] -> [(a,a)]
        go l
            | odd (length l) = error "Incorrect collapseInpNumsIntoInpRanges input size"
            | null l = []
            | otherwise = (head l, head $ tail l): go (tail $ tail l)



applyMultipleRangeMapsOnInput :: [(Int, Int)] -> [[(Int, Int, Int)]] -> [(Int, Int)]
applyMultipleRangeMapsOnInput = foldl'
      (\ inp l -> concatMap (`applyRangeMapOnRangeInput` l) inp)

applyRangeMapOnRangeInput :: (Int, Int) -> [(Int, Int, Int)] -> [(Int, Int)]
applyRangeMapOnRangeInput (_, 0) _ = []
applyRangeMapOnRangeInput t [] = [t]
applyRangeMapOnRangeInput t@(inpStart, inpLen) l@((dstStart,srcStart,rangeLen):ls)
    -- No intersection, inp range ends early
    | inpStart + inpLen - 1 < srcStart = [(inpStart, inpLen)]
    -- No intersection, inp range starts later
    | inpStart > srcStart + rangeLen - 1 =
        applyRangeMapOnRangeInput (inpStart, inpLen) ls
    -- Intersection, inp range starts early
    | inpStart < srcStart =
        (inpStart, srcStart-inpStart)
        : applyRangeMapOnRangeInput (srcStart, inpLen - (srcStart-inpStart)) l
    -- Intersection, inp range ends later
    | inpStart + inpLen - 1 > srcStart + rangeLen - 1 =
        (inpStart - srcStart + dstStart, srcStart + rangeLen - inpStart)
        : applyRangeMapOnRangeInput (srcStart + rangeLen, inpLen - (srcStart + rangeLen - inpStart)) ls
    -- Intersection, inp range completely inside
    | otherwise = [(inpStart - srcStart + dstStart,  inpLen)]

compareRanges :: (Int, Int, Int) -> (Int, Int, Int) -> Ordering
compareRanges (dstStart, srcStart, srcLen) (dstStart', srcStart', srcLen')
    | srcStart < srcStart' = LT
    | srcStart == srcStart' && srcLen <= srcLen' = LT
    | otherwise = GT

collapseIOs :: [IO [(Int, Int, Int)]] -> IO [[(Int, Int, Int)]]
collapseIOs [] = pure []
collapseIOs (a:as) = do
    h <- a
    t <- collapseIOs as
    pure (h:t)

getRangeMapFromFile :: String -> IO [(Int, Int, Int)]
getRangeMapFromFile filename = do
    inputLines <- readFile filename
    pure $ sortBy compareRanges $ map convertLineToRange $ lines inputLines

convertLineToRange :: String -> (Int, Int, Int)
convertLineToRange s = listToThruple $ map read (strSplit (==' ') s)
    where
        listToThruple :: [a] -> (a, a, a)
        listToThruple [a,b,c] = (a,b,c)
        listToThruple _ = error "Incorrect listToThruple input size"

strSplit :: (Char -> Bool) -> String -> [String]
strSplit f s = go [] "" f s
    where
        go :: [String] -> String -> (Char -> Bool) -> String -> [String]
        go res acc _ [] = res ++ [acc]
        go res acc f (c:cs) = if f c then go (res ++ [acc]) "" f cs else go res (acc ++ [c]) f cs

main :: IO ()
main = do
    let inputFile = "seeds.txt"
    let fileOrder = [
            "seedToSoil.txt",
            "soilToFertilizer.txt",
            "fertilizerToWater.txt",
            "waterToLight.txt",
            "lightToTemp.txt",
            "tempToHumid.txt",
            "humidToLoc.txt"
            ]
    sol <- ioSol inputFile fileOrder
    print sol
