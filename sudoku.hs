import Data.List
import Data.List.Split

data Square = Answer Int | Possible [Int] | Suggest Int deriving (Read, Show, Eq)
type Board = [[Square]]

main :: IO ()
main = undefined

getRowFromInput :: [String] -> IO ()
getRowFromInput = undefined

example :: [String]
example = ["090014070",
           "100000400",
           "850073290",
           "070040000",
           "340080065",
           "000020040",
           "039260017",
           "005000006",
           "020150030"
           ]
{-
example = ["900400026",
           "007020150",
           "205010800",
           "304600502",
           "020000080",
           "108007403",
           "003040701",
           "052060900",
           "410009005"]
-}

generateBoard :: [String] -> Board
generateBoard rawStrs = map (\ x -> createRow x) rawStrs
    where 
          createRow :: String -> [Square]
          createRow str = map (\ c -> createSquare c) str
          createSquare :: Char -> Square
          createSquare c = 
              let n = read [c] :: Int
              in if n == 0
                 then Possible [1..9]
                 else Answer n

--solveLoop :: Board -> (Bool, Board)
--solveLoop board = (True, solveRow board)
-- these are part of solveLoop
--solveLoop :: Board -> IO ()
--solveLoop = if checkForWin (generate example)
--	where solve = solveReg . solveCol . solveRow
solveLoop board =
	let process = solveRow . solveCol . solveReg
	in if checkForWin (process board) 
	   then process board 
	   else solveLoop $ process board
	-- if the board is solved, then display.  if not, do solveloop on the processed board

solveRow :: Board -> Board
solveRow board = map consolidate board

solveCol :: Board -> Board
solveCol board = transpose $ map consolidate $ transpose board

solveReg :: Board -> Board
solveReg board = regionalize $ map consolidate $ regionalize board

{-
myList = [[Answer 1, Answer 2, Answer 3, Answer 4, Answer 5, Answer 6, Answer 7, Answer 8, Answer 9],
          [Answer 11, Answer 12, Answer 13, Answer 14, Answer 15, Answer 16, Answer 17, Answer 18, Answer 19],
          [Answer 21, Answer 22, Answer 23, Answer 24, Answer 25, Answer 26, Answer 27, Answer 28, Answer 29],
          [Answer 31, Answer 32, Answer 33, Answer 34, Answer 35, Answer 36, Answer 37, Answer 38, Answer 39],
          [Answer 41, Answer 42, Answer 43, Answer 44, Answer 45, Answer 46, Answer 47, Answer 48, Answer 49],
          [Answer 51, Answer 52, Answer 53, Answer 54, Answer 55, Answer 56, Answer 57, Answer 58, Answer 59],
          [Answer 61, Answer 62, Answer 63, Answer 64, Answer 65, Answer 66, Answer 67, Answer 68, Answer 69],
          [Answer 71, Answer 72, Answer 73, Answer 74, Answer 75, Answer 76, Answer 77, Answer 78, Answer 79],
          [Answer 81, Answer 82, Answer 83, Answer 84, Answer 85, Answer 86, Answer 87, Answer 88, Answer 89]
          ]
-}

regionalize :: Board -> Board
regionalize board = concat $ map regionalizeRow $ splitEvery 3 board
    where
    	  regionalizeRow :: [[Square]] -> [[Square]]
    	  regionalizeRow bigchunk = (first row) : (second row) : (third row) : []
    	      where
    	          first r = r!!0 ++ r!!3 ++ r!!6
    	          second r = r!!1 ++ r!!4 ++ r!!7
    	          third r = r!!2 ++ r!!5 ++ r!!8
    	          row = concat $ map (splitEvery 3) bigchunk
-- divide the board into three row regions
-- let mysplit = concat $ map (splitEvery 3) myList
-- mysplit !! 0 ++ mysplit !! 3 ++ mysplit !! 6
-- concat the new rows

deregionalize :: Board -> Board
deregionalize = undefined

consolidate :: [Square] -> [Square]
consolidate row =
    let convertedRow = map convert row
        answers = getAnswers convertedRow
    in  map (prune answers) convertedRow
    where
          convert :: Square -> Square
          convert (Possible [x]) = (Answer x)
          convert x = x
          getAnswers :: [Square] -> [Int]
          getAnswers [(Answer x)] = [x]
          getAnswers [(Possible x)] = []
          getAnswers ((Answer x):xs) = x : getAnswers xs
          getAnswers ((Possible x):xs) = getAnswers xs
          prune :: [Int] -> Square -> Square
          prune answers (Possible xs) = (Possible (filter (\ x -> not (x `elem` answers)) xs))
          prune answers (Answer x) = (Answer x)
---------------------

checkForWin :: Board -> Bool
checkForWin board = all isAnswer $ concat board
    where
          isAnswer :: Square -> Bool
          isAnswer (Answer _) = True
          isAnswer _          = False

generateSuggestionList :: Board -> [Board]
generateSuggestionList = undefined

displaySolution :: Board -> IO ()
displaySolution = undefined

