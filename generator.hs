-- board generator
import Data.List
import System.Random

seed :: [String]
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

charSwap :: [String] -> Int -> Int -> [String]
charSwap [] _ _ = []
charSwap (x:xs) a b = 
   let a' = head $ show a
       b' = head $ show b
   in (map (\ c -> if c == a' then b' else if c == b' then a' else c) x) : charSwap xs a b

colSwap :: [String] -> Int -> Int -> [String]
colSwap board a b =
	let tBoard = transpose board
	in transpose $ swapLoop tBoard a b (tBoard !! a) (tBoard !! b)

rowSwap :: [String] -> Int -> Int -> [String]
rowSwap board a b = swapLoop board a b (board !! a) (board !! b)

swapLoop :: [String] -> Int -> Int -> String -> String -> [String]
swapLoop [] _ _ _ _ = []
swapLoop (x:xs) a b ath bth
   | length xs == 8 - a = bth : swapLoop xs a b ath bth
   | length xs == 8 - b = ath : swapLoop xs a b ath bth
   | otherwise          = x   : swapLoop xs a b ath bth

generateBoard' :: [String] -> Int -> [String]
generateBoard' seed n = genBoardLoop seed n (randomFunctions n 374657826458726) (randomParams (2 * n) 7829482375982)

genBoardLoop :: [String] -> Int -> [Int] -> [Int] -> [String]
genBoardLoop board n _ [] = board
genBoardLoop board n [] _ = board
genBoardLoop board n (f:fs) (p:p':ps)
   | f == 1    = genBoardLoop (colSwap board (p - 1) (p' - 1)) n fs ps 
   | f == 2    = genBoardLoop (rowSwap board (p - 1) (p' - 1)) n fs ps 
   | otherwise = genBoardLoop (charSwap board p p') n fs ps 

randomParams :: Int -> Int -> [Int]
randomParams n seed' = take n . randomRs (1, 9) . mkStdGen $ seed'

randomFunctions :: Int -> Int -> [Int]
randomFunctions n seed' = take n . randomRs (1, 3) . mkStdGen $ seed'

