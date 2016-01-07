-- board generator
import Data.List

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