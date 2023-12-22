module Main where
import Data.Char (isDigit, digitToInt, isSymbol, isLetter)

getFrom2DList :: [[a]] -> Int -> Int -> a
getFrom2DList b r c = b !! r !! c

-- >>> findPartNumsInLine ["78*8", "...."] 0
-- 86

-- >>> getNum $ drop 0 (["78*8", "...."] !! 0)
-- (78,2)

findPartNumsInLine :: [String] -> Int -> Int
findPartNumsInLine board rowInd = go 0 0
    where
        go :: Int -> Int -> Int
        go colInd acc
            | colInd >= numCols = acc
            | not $ isDigit $ getFrom2DList board rowInd colInd = go (colInd + 1) acc
            | otherwise =
                let (num, numChars) = getNum $ drop colInd (board !! rowInd) in
                let endColId = colInd + numChars in
                if isPart rowInd colInd (endColId-1) then go (endColId+1) (acc+num) else go (endColId+1) (acc)
                where
                    isPart :: Int -> Int -> Int -> Bool
                    isPart row startCol endCol =
                        go (
                            [(row, startCol-1), (row, endCol+1)] ++
                            map (row+1,) [startCol .. endCol] ++
                            map (row-1,) [startCol .. endCol] ++
                            [(row-1, startCol-1), (row+1, startCol-1), (row-1, endCol+1), (row+1, endCol+1)])
                        where
                            go :: [(Int, Int)] -> Bool
                            go [] = False
                            go ((row,col):ls) =
                                    ((row >= 0) &&
                                    (row < numRows) &&
                                    (col >= 0) &&
                                    (col < numCols) &&
                                    (c /= '.') &&
                                    not (isLetter c) &&
                                    not (isDigit c)) || go ls
                                    where c = getFrom2DList board row col
        numCols = length (board !! rowInd)
        numRows = length board

getNum :: [Char] -> (Int, Int)
getNum l = go 0 0 l
    where
        go acc numCs [] = (acc, numCs)
        go acc numCs (c:cs) = if isDigit c then go (acc*10 + digitToInt c) (numCs+1) cs else (acc, numCs)

sol :: [String] -> Int
sol board = sum $ map (findPartNumsInLine board) [0..(length board-1)]


main :: IO ()
main = do
    inputLines <- readFile "input.txt"
    let board = lines inputLines
    print $ map (findPartNumsInLine board) [0..(length board-1)]
    print $ sol (lines inputLines)
