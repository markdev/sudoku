import Data.List

data Square = Answer Int | Possible [Int] | Suggest Int deriving (Read, Show, Eq)
type Board = [[Square]]

main :: IO ()
main = undefined

getRowFromInput :: [String] -> IO ()
getRowFromInput = undefined

example :: [String]
example = ["900400026",
           "007020150",
           "205010800",
           "304600502",
           "020000080",
           "108007403",
           "003040701",
           "052060900",
           "410009005"]

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
solveLoop = solveReg $ solveCol $ solveRow $ generateBoard example

solveRow :: Board -> Board
solveRow board = map consolidate board

solveCol :: Board -> Board
solveCol board = transpose $ map consolidate $ transpose board

solveReg :: Board -> Board
solveReg board = board

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

{-
data Square = Answer Int | Possible [Int] | Suggest Int deriving (Read, Show, Eq)
type Board = [[Square]]

myRow :: [Square]
myRow = [Answer 5, Answer 1, Possible [4], Answer 7, Answer 2, Possible [1,2,3,4,5,6,7,8,9], Answer 4, Possible [1,2,3,4,5,6,7,8,9], Answer 9]
-}
