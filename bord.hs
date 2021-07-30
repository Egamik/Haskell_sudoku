module KojunBoard where
    import Data.Tuple
    import Data.List
    import Data.Array.Unboxed

    type CellValue = Int                        -- Valor guardado na célula.
    type GroupId = Int                          -- ID do grupo em que a célula está.
    type Position = (Int, Int)                  -- Posição (x, y) da célula.
    type Cell = (Position, CellValue, GroupId)  -- Representação de uma célula do tabuleiro.
    type GameBoard = UArray Cell                -- Tabuleiro do jogo.
    type Group = [Cell]                         -- Grupo de células do tabuleiro.
    
    gameBoard :: GameBoard
    gameBoard = array ((0,0), (9,9)) $ puzzleAssocs examplePuzzle

    examplePuzzle :: [[Value]]
    examplePuzzle = [[5, 0, 2, 0, 2, 0, 3, 1, 3, 1],
                    [0, 4, 0, 1, 0, 5, 0, 5, 0, 4],
                    [7, 5, 1, 7, 0, 0, 3, 1, 3, 0],
                    [0, 4, 0, 0, 0, 0, 0, 0, 0, 3],
                    [2, 0, 3, 4, 0, 2, 0, 0, 4, 0],
                    [5, 0, 2, 0, 6, 0, 0, 0, 0, 0],
                    [0, 1, 3, 0, 1, 0, 0, 4, 0, 3],
                    [6, 7, 0, 3, 0, 1, 4, 0, 0, 1],
                    [4, 0, 3, 0, 4, 0, 0, 0, 0, 3],
                    [0, 1, 0, 2, 0, 6, 2, 0, 2, 1]]
    exampleGroups :: [(Int, [(Int, Int)])]
    exampleGroups = [(1, [(0,0), (1,0), (1,1), (1,2), (3,2)]),
                    (2, [(0,1), (0,2), (0,3), (1,3)]),
                    (3, [(0,4), (0,5), (0,6), (0,7)]),
                    (4, [(0,8), (0,9)]),
                    ....]

    -- Retorna lista de posicoes vazias
    emptyPositions :: GameBoard -> [Position]
    emptyPositions b = [(row, col) | row <- [0..9], col <- [0..9], b ! (row, col) == 0]

    -- Retorna valores dentro de um grupo dada uma posicao
    -- usar para ver quais valores sao validos
    valuesInGroup :: GameBoard -> Position -> Groups -> [Value]
    valuesInGroup b (row, col) gr = [b ! loc | loc <- locations] where 
        --

    -- Retorna valores adjacentes a posicao
    valuesInAdjancy :: GameBoard -> Position -> [Value]

    -- Determina se valor eh valido
    isPossibleValue :: Value -> Position -> GameBoard -> Bool

    -- Converte um grupo em um vetor associativo
    puzzleAssocs :: [[Value]] -> [(Position, Value)]
    puzzleAssocs = concatMap rowAssocs . zip [0..9]
      where
        rowAssocs :: (Int, [Value]) -> [((Int, Int), Position)]
        rowAssocs (row, marks) = colAssocs row $ zip [0..9] marks

        colAssocs :: Int -> [(Int, Value)] -> [((Int, Int), Value)]
        colAssocs row cols = map (\(col, m) -> ((row, col), m)) cols