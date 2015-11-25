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
    where createRow str = map (\ c -> createSquare c) str
          createSquare c = 
              let n = read [c] :: Int
              in if n == 0
                 then Possible [1..9]
                 else Answer n

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

