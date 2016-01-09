import Data.List
import Data.List.Split
import Generator

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
regionalize board = concat $ map regionalizeRow $ splitEvery 3 board
    where
         regionalizeRow :: [[Square]] -> [[Square]]
         regionalizeRow bigchunk = (first row) : (second row) : (third row) : []
             where
                  first r = r!!0 ++ r!!3 ++ r!!6
                  second r = r!!1 ++ r!!4 ++ r!!7
                  third r = r!!2 ++ r!!5 ++ r!!8
                  row = concat $ map (splitEvery 3) bigchunk

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
makeBoardStrings board = intercalate ["================="] $ splitEvery 3 $ map (intersperse '|' . stringify) board
    where
        stringify :: [Square] -> String
        stringify [] = []
        stringify [(Answer x)] = (show x :: String)
        stringify ((Answer x):xs) = (show x :: String) ++ stringify xs
    

