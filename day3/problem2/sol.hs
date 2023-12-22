module Main where
import Data.Char (isDigit, digitToInt, isSymbol, isLetter)
import Data.Map.Strict as Map

type StarToNums = Map (Int, Int) [Int]

getFrom2DList :: [[a]] -> Int -> Int -> a
getFrom2DList b r c = b !! r !! c

-- >>> let b = ["*8*0*", ".***."] in (findPartNumsInLine b 1 (findPartNumsInLine b 0 empty))
-- fromList [((0,0),[8]),((0,2),[8,0]),((0,4),[0]),((1,1),[8]),((1,2),[8,0]),((1,3),[0])]

findPartNumsInLine :: [String] -> Int -> StarToNums -> StarToNums
findPartNumsInLine board rowInd m = go 0 m
    where
        go :: Int -> StarToNums -> StarToNums
        go colInd m'
            | colInd >= numCols = m'
            | not $ isDigit $ getFrom2DList board rowInd colInd = go (colInd + 1) m'
            | otherwise =
                let (num, numChars) = getNum $ Prelude.drop colInd (board !! rowInd) in
                let endColId = colInd + numChars in
                    go (endColId+1) (updateStars m' num (getNeighborStarsList rowInd colInd endColId))
                where
                    updateStars :: StarToNums -> Int -> [(Int, Int)] -> StarToNums
                    updateStars curmap num [] = curmap
                    updateStars curmap num (s:ss) = updateStars (mapinsert curmap s num) num ss
                    mapinsert :: StarToNums -> (Int, Int) -> Int -> StarToNums
                    mapinsert curMap key num =
                        case Map.lookup key curMap of
                            Just prevNums -> insert key (prevNums ++ [num]) curMap
                            Nothing -> insert key [num] curMap
                    isValidStarPos :: (Int, Int) -> Bool
                    isValidStarPos (r,c) = r >= 0 && r < numRows && c >= 0 && c < numCols && getFrom2DList board r c == '*'
                    getNeighborStarsList :: Int -> Int -> Int -> [(Int, Int)]
                    getNeighborStarsList rowInd colInd endColId = 
                        Prelude.filter isValidStarPos
                        ([(rowInd, colInd-1), (rowInd, endColId)] ++
                        Prelude.map (rowInd+1,) [colInd .. (endColId-1)] ++
                        Prelude.map (rowInd-1,) [colInd .. (endColId-1)] ++
                        [(rowInd-1, colInd-1), (rowInd+1, colInd-1), (rowInd-1, endColId), (rowInd+1, endColId)])
        numCols = length (board !! rowInd)
        numRows = length board

getNum :: [Char] -> (Int, Int)
getNum l = go 0 0 l
    where
        go acc numCs [] = (acc, numCs)
        go acc numCs (c:cs) = if isDigit c then go (acc*10 + digitToInt c) (numCs+1) cs else (acc, numCs)

sol :: [String] -> Int
sol board = Map.foldr' (\v acc -> combinevals v + acc) 0 (Map.filter (\v -> length v == 2) (go 0 empty))
    where
        combinevals :: [Int] -> Int
        combinevals = go 1
            where
                go :: Int -> [Int] -> Int
                go acc [] = acc
                go acc (l:ls) = go (acc*l) ls
        go :: Int -> StarToNums -> StarToNums
        go n m
            | n == length board = m
            | otherwise = go (n+1) (findPartNumsInLine board n m)


main :: IO ()
main = do
    inputLines <- readFile "input.txt"
    print $ sol (lines inputLines)
