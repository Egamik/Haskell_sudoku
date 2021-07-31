import Data.Tuple
import Data.List

type CellValue = Int
type GroupId = Int
type Position = (Int, Int)
type Cell = (Position, CellValue, GroupId)
type GameBoard = [Cell]
type Group = [Cell]

gameBoardSize = 3

getPosition :: Cell -> Position
getPosition (position, _, _) = position

getCellValue :: Cell -> CellValue
getCellValue (_, value, _) = value

getGroupId :: Cell -> GroupId
getGroupId (_, _, groupId) = groupId

printGameBoard :: GameBoard -> Int -> IO()
printGameBoard gameBoard column = do
    putStr (show (getCellValue (head gameBoard)))
    if (mod column 3) == 2
        then putStr "\n"
    else putStr " "
    printGameBoard (tail gameBoard) (snd (getPosition (head (tail gameBoard))))

main = do
    let gameBoard = [((0, 0), 1, 1), ((0, 1), 2, 1), ((0, 2), 3, 3),
                     ((1, 0), 4, 2), ((1, 1), 5, 1), ((1, 2), 6, 2),
                     ((2, 0), 7, 2), ((2, 1), 8, 2), ((2, 2), 9, 2)]
    printGameBoard gameBoard 0
