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

    getTopPosition :: Position -> Maybe Position
    getTopPosition (0, j) = Nothing
    getTopPosition (i, j) = Just (i - 1, j)

    getBottomPosition :: Position -> Maybe Position
    getBottomPosition (9, j) = Nothing
    getBottomPosition (i, j) = Just (i + 1, j)

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

    getTopCell :: Board -> Cell -> Maybe Cell
    getTopCell board cell = getCell board (getTopPosition (getPosition cell))

    getBottomCell :: Board -> Cell -> Maybe Cell
    getBottomCell board cell = getCell board (getBottomPosition (getPosition cell))

    getLeftCell :: Board -> Cell -> Maybe Cell
    getLeftCell board cell = getCell board (getLeftPosition (getPosition cell))

    getRightCell :: Board -> Cell -> Maybe Cell
    getRightCell board cell = getCell board (getRightPosition (getPosition cell))

    -- Função que retorna uma célula adjacente.
    -- Parâmetros:
    ---- Board: tabuleiro.
    ---- (Position -> Maybe Position): função de posição em relação à célula.
    ---- Cell: célula.
    -- Retorno:
    ---- Maybe Cell: célula adjacente à célula original.
    getAdjacentCell :: Board -> (Position -> Maybe Position) -> Cell -> Maybe Cell
    getAdjacentCell board positionFunction cell = getCell board (positionFunction (getPosition cell))