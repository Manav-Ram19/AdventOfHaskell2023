module Main where
import Data.Foldable (Foldable(foldl'))


ioSol :: String -> [String] -> IO Int
ioSol inputFile mapFiles = do
    rangeMaps <- collapseIOs $ map getRangeMapFromFile mapFiles
    inputs <- getInputsFromFile inputFile
    pure $ foldl' (\acc v -> min acc (applyMultipleRangeMapsOnInput v rangeMaps)) (maxBound :: Int) inputs

getInputsFromFile :: String -> IO [Int]
getInputsFromFile inpFileName = do
    inputLines <- readFile inpFileName
    pure $ map read $ strSplit (==' ') $ head $ lines inputLines

applyMultipleRangeMapsOnInput :: Int -> [[(Int, Int, Int)]] -> Int
applyMultipleRangeMapsOnInput = foldl' applyRangeMapOnInput

applyRangeMapOnInput :: Int -> [(Int, Int, Int)] -> Int
applyRangeMapOnInput inp [] = inp
applyRangeMapOnInput inp ((dstStart,srcStart,rangeLen):ls)
    | inp >= srcStart && inp <= srcStart + rangeLen = dstStart + (inp - srcStart)
    | otherwise = applyRangeMapOnInput inp ls

collapseIOs :: [IO [(Int, Int, Int)]] -> IO [[(Int, Int, Int)]]
collapseIOs [] = pure []
collapseIOs (a:as) = do
    h <- a
    t <- collapseIOs as
    pure (h:t)

getRangeMapFromFile :: String -> IO [(Int, Int, Int)]
getRangeMapFromFile filename = do
    inputLines <- readFile filename
    pure $ map convertLineToRange $ lines inputLines

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
