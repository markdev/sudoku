import Data.List
import Data.List.Split
import Generator

data Square = Answer Int | Possible [Int] | Suggest Int deriving (Read, Show, Eq)
type Board = [[Square]]
type RowIndex = Int

main :: IO ()
main = do
  putStrLn openingText
  mainMenu

openingText :: String
openingText = "Welcome to my amazing Sudoku machine!\n"

mainMenu :: IO ()
mainMenu = do
  putStrLn ("What do you want to do?\n" ++ "Enter 1 to enter a sudoku\n" ++ "Enter 2 to generate one automatically\n")
  choice <- getLine
  case choice of 
    "1" -> enterSudoku
    "2" -> generateSudoku 
    otherwise -> do
      putStrLn "That is not a valid response"
      mainMenu


-- Option 1: enter sudoku
enterSudoku :: IO ()
enterSudoku = do
  putStrLn "I'm going to prompt you to enter the sudoku row by row.  Don't you just love command lines?"
  putStrLn "Starting from row 1, enter a nine-digit number, substituting \"0\" for an unsolved square."
  putStrLn "Example: \"||  | 2 |  || 7 |   | 9 || 3 |  |  ||\" would be entered as \"020709300\""
  putStrLn "Capisce?  Good."
  enterRow 0 [[]]

enterRow :: RowIndex -> [String] -> IO ()
enterRow i board
  | i >= 9 = solveScreen $ tail board
  | otherwise = do
    putStrLn ("Enter a nine-digit number for row " ++ (show (i + 1)) ++ ": ")
    rowString <- getLine
    if isValidRow rowString then
      enterRow (i + 1) (board ++ [rowString])
    else do
      putStrLn "That is not a valid row, try again."
      enterRow i board

isValidRow :: String -> Bool
isValidRow xs = (all (`elem` ['0'..'9']) xs) && (length xs == 9)


-- Option 2: generate sudoku
generateSudoku :: IO ()
generateSudoku = do
  putStrLn "Enter some big random number, like 238764867 (yes, really)"
  hash <- getLine
  if isANumber hash then
    solveScreen (makeRandomBoard seed 200 (read hash :: Int))
  else do
    putStrLn "That is not a good number."
    generateSudoku

isANumber :: String -> Bool
isANumber xs = all (`elem` ['0'..'9']) xs


-- Display the sudoku
solveScreen :: [String] -> IO ()
solveScreen rows = do
  putStrLn $ displayUnsolved rows 
  putStrLn "Okay, are you ready to solve this thing? (Press enter)"
  ready <- getLine
  solve $ generateBoard rows 

displayUnsolved :: [String] -> String
displayUnsolved rows = concat $ intercalate ["---------------------\n"] $ chunksOf 3 $ map rowify rows
  where
    rowify xs = (intersperse ' ' $ intercalate "|" $ chunksOf 3 xs) ++ "\n" 


-- Solve this thing 
solve :: Board -> IO ()
solve board = do
  solveLoop board



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

solveLoop :: Board -> IO ()
solveLoop board =
    let processed = solveRow $ solveCol $ solveReg board
    in if checkForWin processed 
       then displaySolution processed
       else solveLoop $ processed

solveRow :: Board -> Board
solveRow board = map consolidate board

solveCol :: Board -> Board
solveCol board = transpose $ map consolidate $ transpose board

solveReg :: Board -> Board
solveReg board = regionalize $ map consolidate $ regionalize board

regionalize :: Board -> Board
regionalize board = concat $ map regionalizeRow $ chunksOf 3 board
    where
         regionalizeRow :: [[Square]] -> [[Square]]
         regionalizeRow bigchunk = (first row) : (second row) : (third row) : []
             where
                  first r = r!!0 ++ r!!3 ++ r!!6
                  second r = r!!1 ++ r!!4 ++ r!!7
                  third r = r!!2 ++ r!!5 ++ r!!8
                  row = concat $ map (chunksOf 3) bigchunk

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

checkForWin :: Board -> Bool
checkForWin board = all isAnswer $ concat board
    where
          isAnswer :: Square -> Bool
          isAnswer (Answer _) = True
          isAnswer _          = False

generateSuggestionList :: Board -> [Board]
generateSuggestionList = undefined

displaySolution :: Board -> IO ()
displaySolution board = do
    mapM_ print $ makeBoardStrings board
    
makeBoardStrings :: Board -> [String]
makeBoardStrings board = intercalate ["================="] $ chunksOf 3 $ map (intersperse '|' . stringify) board
    where
        stringify :: [Square] -> String
        stringify [] = []
        stringify [(Answer x)] = (show x :: String)
        stringify ((Answer x):xs) = (show x :: String) ++ stringify xs
    

