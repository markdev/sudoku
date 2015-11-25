data Square = Answer Int | Possible [Int] | Suggest Int
type Board = [[Square]]

main :: IO ()
main = undefined

getRowFromInput :: [String] -> IO ()
getRowFromInput = undefined

example :: [[Int]]
example = undefined

generateBoard :: [[Int]] -> Board
generateBoard = undefined

solveLoop :: Board -> (Bool, Board)
solveLoop = undefined
-- these are part of solveLoop
solveRow :: Board -> (Bool, Board)
solveRow = undefined

solveCol :: Board -> (Bool, Board)
solveCol = undefined

solveReg :: Board -> (Bool, Board)
solveReg = undefined
---------------------

checkForWin :: Board -> Bool
checkForWin = undefined

generateSuggestionList :: Board -> [Board]
generateSuggestionList = undefined

displaySolution :: Board -> IO ()
displaySolution = undefined

