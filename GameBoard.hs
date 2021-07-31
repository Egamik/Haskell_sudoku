module GameBoard where
    type CellValue = Int
    type GroupId = Int
    type Position = (Int, Int)
    type Cell = (Position, CellValue, GroupId)
    type Board = [Cell]
    type Group = [Cell]

    getPosition :: Cell -> Position
    getPosition (position, _, _) = position

    getCellValue :: Cell -> CellValue
    getCellValue (_, value, _) = value

    getGroupId :: Cell -> GroupId
    getGroupId (_, _, groupId) = groupId

    getUpperPosition :: Position -> Maybe Position
    getUpperPosition (0, j) = Nothing
    getUpperPosition (i, j) = Just (i - 1, j)

    getLowerPosition :: Position -> Maybe Position
    getLowerPosition (9, j) = Nothing
    getLowerPosition (i, j) = Just (i + 1, j)

    getLeftPosition :: Position -> Maybe Position
    getLeftPosition (i, 0) = Nothing
    getLeftPosition (i, j) = Just (i, j - 1)

    getRightPosition :: Position -> Maybe Position
    getRightPosition (i, 9) = Nothing
    getRightPosition (i, j) = Just (i, j + 1)

    getCell :: Board -> Maybe Position -> Maybe Cell
    getCell board position = do
        if (length board) == 0
            then Nothing
        else if Just (getPosition (head board)) == position
            then Just (head board)
        else getCell (tail board) position

    getUpperCell :: Board -> Cell -> Maybe Cell
    getUpperCell board cell = getCell board (getUpperPosition (getPosition cell))

    getLowerCell :: Board -> Cell -> Maybe Cell
    getLowerCell board cell = getCell board (getLowerPosition (getPosition cell))

    getLeftCell :: Board -> Cell -> Maybe Cell
    getLeftCell board cell = getCell board (getLeftPosition (getPosition cell))

    getRightCell :: Board -> Cell -> Maybe Cell
    getRightCell board cell = getCell board (getRightPosition (getPosition cell))