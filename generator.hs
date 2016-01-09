-- QUESTION: rowswap/colswap take 2 parameters, which each seem to be random integers 1-9. but swapping rows 3 and 4, for example, can create an unsolvable puzzle. how do we know that the swap functions are only operating on pairs 1-3, 4-6, or 7-9?

module Generator (
  seed, 
  charSwap,
  colSwap,
  rowSwap,
  swapLoop,
  diagonalSwap,
  makeRandomBoard,
  genBoardLoop,
  randomParams,
  randomFunctions
  ) where

import Data.List
import Data.Char(intToDigit)
import System.Random

type Sudoku = [String]
type Gibberish = Int

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
   let a' = intToDigit a
       b' = intToDigit b
   in (map (\ c -> if c == a' then b' else if c == b' then a' else c) x) : charSwap xs a b

colSwap :: Sudoku -> (Int, Int) -> Sudoku
colSwap board (a, b) =
   let tBoard = transpose board
   in transpose $ swapLoop tBoard a b (tBoard !! a) (tBoard !! b)

rowSwap :: Sudoku -> (Int, Int) -> Sudoku
rowSwap board (a, b) = swapLoop board a b (board !! a) (board !! b)

swapLoop :: Sudoku -> Int -> Int -> String -> String -> Sudoku
swapLoop [] _ _ _ _ = []
swapLoop (x:xs) a b ath bth
   | length xs == 8 - a = bth : swapLoop xs a b ath bth
   | length xs == 8 - b = ath : swapLoop xs a b ath bth
   | otherwise          = x   : swapLoop xs a b ath bth
   
diagonalSwap :: Sudoku -> Sudoku
diagonalSwap [] = []
diagonalSwap xs = foldl1 (zipWith (++)) $ map rowToCol xs
    where 
        rowToCol :: String -> [String]
        rowToCol [] = []
        rowToCol (x:xs) = [x]:(rowToCol xs)

makeRandomBoard :: Sudoku -> Int -> Gibberish -> Sudoku
makeRandomBoard seed n gibberish = genBoardLoop seed n (randomFunctions n gibberish) (randomParams (2 * n) gibberish)

genBoardLoop :: Sudoku -> Int -> [Int] -> [Int] -> Sudoku
genBoardLoop board n _ [] = board
genBoardLoop board n [] _ = board
genBoardLoop board n (f:fs) (p:p':ps)
   | f == 1    = genBoardLoop (colSwap board (getSwapPair p)) n fs ps 
   | f == 2    = genBoardLoop (rowSwap board (getSwapPair p)) n fs ps 
   | f == 3    = genBoardLoop (charSwap board p p') n fs ps 
   | otherwise = genBoardLoop (diagonalSwap board) n fs ps 

getSwapPair :: Int -> (Int, Int)
getSwapPair n = case n of
   1 -> (0,1)
   2 -> (0,2)
   3 -> (1,2)
   4 -> (3,4)
   5 -> (3,5)
   6 -> (4,5)
   7 -> (6,7)
   8 -> (6,8)
   9 -> (7,8)

randomParams :: Int -> Gibberish -> [Int]
randomParams n seed' = take n . randomRs (1, 9) . mkStdGen $ seed'

randomFunctions :: Int -> Gibberish -> [Int]
randomFunctions n seed' = take n . randomRs (1, 4) . mkStdGen $ seed'

