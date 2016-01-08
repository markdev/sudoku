-- board generator

-- QUESTION: rowswap/colswap take 2 parameters, which each seem to be random integers 1-9. but swapping rows 3 and 4, for example, can create an unsolvable puzzle. how do we know that the swap functions are only operating on pairs 1-3, 4-6, or 7-9?

import Data.List
import System.Random

type Sudoku = [String]

seed :: Sudoku
seed = ["090014070",
        "100000400",
        "850073290",
        "070040000",
        "340080065",
        "000020040",
        "039260017",
        "005000006",
        "020150030"
       ]

charSwap :: Sudoku -> Int -> Int -> Sudoku
charSwap [] _ _ = []
charSwap (x:xs) a b = 
   let a' = head $ show a
       b' = head $ show b
   in (map (\ c -> if c == a' then b' else if c == b' then a' else c) x) : charSwap xs a b

colSwap :: Sudoku -> Int -> Int -> Sudoku
colSwap board a b =
	let tBoard = transpose board
	in transpose $ swapLoop tBoard a b (tBoard !! a) (tBoard !! b)

rowSwap :: Sudoku -> Int -> Int -> Sudoku
rowSwap board a b = swapLoop board a b (board !! a) (board !! b)

swapLoop :: Sudoku -> Int -> Int -> String -> String -> Sudoku
swapLoop [] _ _ _ _ = []
swapLoop (x:xs) a b ath bth
   | length xs == 8 - a = bth : swapLoop xs a b ath bth
   | length xs == 8 - b = ath : swapLoop xs a b ath bth
   | otherwise          = x   : swapLoop xs a b ath bth
   
diagonalSwap :: Sudoku -> Sudoku
diagonalSwap [] = []
diagonalSwap xs = foldl1 (zipWith (++)) $ map rowToCol xs

rowToCol :: String -> [String]
rowToCol [] = []
rowToCol (x:xs) = [x]:(rowToCol xs)

generateBoard' :: Sudoku -> Int -> Sudoku
generateBoard' seed n = genBoardLoop seed n (randomFunctions n 374657826458726) (randomParams (2 * n) 7829482375982)

genBoardLoop :: Sudoku -> Int -> [Int] -> [Int] -> Sudoku
genBoardLoop board n _ [] = board
genBoardLoop board n [] _ = board
genBoardLoop board n (f:fs) (p:p':ps)
   | f == 1    = genBoardLoop (colSwap board (p - 1) (p' - 1)) n fs ps 
   | f == 2    = genBoardLoop (rowSwap board (p - 1) (p' - 1)) n fs ps 
   | f == 3    = genBoardLoop (charSwap board p p') n fs ps 
   | otherwise = genBoardLoop (diagonalSwap board) n fs ps 

randomParams :: Int -> Int -> [Int]
randomParams n seed' = take n . randomRs (1, 9) . mkStdGen $ seed'

randomFunctions :: Int -> Int -> [Int]
randomFunctions n seed' = take n . randomRs (1, 4) . mkStdGen $ seed'

