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

    -- Busca uma célula no tabuleiro, se houver.
    -- Parâmetros:
    ---- Board: tabuleiro.
    ---- Maybe Position: posição desejada para encontrar a célula.
    getCell :: Board -> Maybe Position -> Maybe Cell
    getCell board position = do
        if null board
            then Nothing
        else if Just (getPosition (head board)) == position
            then Just (head board)
        else getCell (tail board) position

    -- Função que retorna uma célula adjacente.
    -- Parâmetros:
    ---- Board: tabuleiro.
    ---- (Position -> Maybe Position): função de posição em relação à célula.
    ---- Cell: célula.
    -- Retorno:
    ---- Maybe Cell: célula adjacente à célula original.
    getAdjacentCell :: Board -> (Position -> Maybe Position) -> Cell -> Maybe Cell
    getAdjacentCell board positionFunction cell = getCell board (positionFunction (getPosition cell))

    -- Indica se duas células estão em um mesmo grupo.
    -- Parâmetros:
    ---- Cell: célula 1.
    ---- Cell: célula 2.
    -- Retorno:
    ---- Bool: se as duas células estiverem no mesmo grupo, true, caso contrário false.
    isInSameGroup :: Cell -> Cell -> Bool
    isInSameGroup cell1 cell2 = getGroupId cell1 == getGroupId cell2

    -- Indica se as células adjacentes tem valor diferente da célula central.
    -- Parâmetros:
    ---- Board: tabuleiro.
    ---- Cell: célula central.
    -- Retorno:
    ---- True, se todas as células adjacentes tem valores diferentes. False, caso contrário.
    hasDifferentAdjacentCells :: Board -> Cell -> Bool
    hasDifferentAdjacentCells board cell = do
        let topDifference = isDifferentFrom board getTopPosition cell
        let bottomDifference = isDifferentFrom board getBottomPosition cell
        let leftDifference = isDifferentFrom board getLeftPosition cell
        let rightDifference = isDifferentFrom board getRightPosition cell
        and [topDifference, bottomDifference, leftDifference, rightDifference]

    -- Indica se uma célula em determinada posição tem valor diferente da célula original.
    -- Parâmetros:
    ---- Board: tabuleiro.
    ---- Cell: célula original.
    -- Retorno:
    ---- True, se célula em determinada posição tem valor diferente. False, caso contrário.
    isDifferentFrom :: Board -> (Position -> Maybe Position) -> Cell -> Bool
    isDifferentFrom board positionFunction cell = do
        case getAdjacentCell board positionFunction cell of
            Just otherCell -> hasDifferentValues cell otherCell
            Nothing -> True

    -- Indica se duas células tem valores diferentes.
    -- Parâmetros:
    ---- Cell: célula 1.
    ---- Cell: célula 2.
    -- Retorno:
    ---- True, se as células tem valores diferentes. False, caso contrário.
    hasDifferentValues :: Cell -> Cell -> Bool 
    hasDifferentValues a b = getCellValue a /= getCellValue b

    -- Indica se a célula é válida para a solução do jogo.
    -- Parâmetros:
    ---- Board: tabuleiro.
    ---- Cell: célula a ser analisada.
    -- Retorno:
    ---- True, se a célula é válida. False, caso contrário.
    isValidSolutionCell :: Board -> Cell -> Bool 
    isValidSolutionCell board cell = do
        let adjacencyValid = hasDifferentAdjacentCells board cell
        let topCellValid = isTopCellValid board cell
        adjacencyValid && topCellValid
        
    -- Indica se a regra da célula mais acima está sendo atendida válida.
    -- Parâmetros:
    ---- Board: tabuleiro.
    ---- Cell: célula a ser analisada.
    -- Retorno:
    ---- True, se a regra é atendida. False, caso contrário.
    isTopCellValid :: Board -> Cell -> Bool 
    isTopCellValid board cell = do
        case getAdjacentCell board getTopPosition cell of
            Just a -> (isInSameGroup a cell && getCellValue a > getCellValue cell) || not (isInSameGroup a cell)
            Nothing -> True

    -- Função que imprime o tabuleiro na tela.
    -- Parâmetros:
    ---- Board: tabuleiro.
    ---- Int: coluna inicial a partir de onde o tabuleiro deve ser inserido (o valor comum é zero).
    ---- Int: ordem da matriz que representa o tabuleiro.
    printGameBoard :: Board -> Int -> Int -> IO()
    printGameBoard board column boardSize = do
        if null board
            then putStr ""
        else do
            let currentCell = head board
            putStr (show (getCellValue currentCell) ++ "(" ++ show (getGroupId currentCell) ++ ") ")
            if mod column boardSize == boardSize - 1
                then putStr "\n"
            else putStr ""
            printGameBoard (tail board) (snd (getPosition (head (tail board)))) boardSize
    
    -- Retorna uma string apresentando a posição de uma célula.
    -- Parâmetros:
    ---- Cell: célula a ter sua posição apresentada.
    showPosition :: Cell -> String
    showPosition cell = "Position: " ++ show (getPosition cell)

    -- Retorna uma string apresentando o valor de uma célula.
    -- Parâmetros:
    ---- Cell: célula a ter seu valor apresentado.
    showCellValue :: Cell -> String 
    showCellValue cell = "Value: " ++ show (getCellValue cell)

    -- Retorna uma string apresentando o grupo de uma célula.
    -- Parâmetros:
    ---- Cell: célula a ter seu grupo apresentado.
    showGroup :: Cell -> String 
    showGroup cell = "Group: " ++ show (getGroupId cell)

    -- Imprime as informações de uma célula.
    -- Parâmetros:
    ---- Cell: célula a ter suas informações apresentadas.
    printCell :: Cell -> IO()
    printCell cell = do
        print (showPosition cell ++ " - " ++ showCellValue cell ++ " - " ++ showGroup cell)