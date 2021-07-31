module KojunBoard where
  
  type CellValue = Int                          -- Valor guardado na celula
  type Cell = (CellValue, GroupId)
  type GroupId = Int                            -- Indexes dos membros de um grupo 
  type Group = [GroupId]                        -- Grupos de posicoes
  type Board = [Cell]
  
  examplePuzzle :: [[Int]]
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
  
  exampleGroups :: [[Int]]
  exampleGroups = [[1,  2,  2,  2,  3,  3,  3,  3,  4,  4],
                  [ 1,  1,  1,  2,  6,  6,  8,  8,  4,  8],
                  [ 5,  5,  1,  6,  6,  7,  9,  8,  8,  8],
                  [ 5,  5,  6,  6,  10, 7,  9,  9,  9,  12],
                  [ 5,  5,  5,  6,  10, 10, 11, 12, 12, 12],
                  [ 13, 13, 14, 14, 14, 10, 15, 15, 16, 16],
                  [ 13, 13, 13, 14, 14, 18, 19, 20, 16, 16],
                  [ 17, 17, 13, 14, 18, 18, 19, 20, 21, 21],
                  [ 17, 17, 22, 22, 22, 22, 19, 20, 20, 23],
                  [ 17, 17, 17, 22, 22, 22, 19, 19, 23, 23]] 

  -- Transforma matriz em lista
  linearizaMatriz :: [[Int]] -> [Int]
  linearizaMatriz [] = []
  linearizaMatriz (h:t) = h ++ (linearizaMatriz t)

  -- Recebe duas matrizes linearizadas e faz pares
  juntaMatriz :: [Int] -> [Int] -> [(Int, Int)]
  juntaMatriz [] _ = []
  juntaMatriz _ [] = []
  juntaMatriz a b = zip a b

  {- Recebe ID do grupo
  -}
  tamanhoGrupo :: [Group] -> Int
  tamanhoGrupo _ = 0
  tamanhoGrupo xs
      | head xs == 0 = 1 + tamanhoGrupo a (tail xs)
      | otherwise = tamanhoGrupo a (tail xs)
  
  -- Recebe numer, um grupo e retorna true caso exista o valor no grupo
  grupoPossui :: CellValue -> [Int] -> Bool 
  grupoPossui _ [] = False
  grupoPossui 0 [] = False
  grupoPossui valor (h:t)
      | h == valor = True
      | otherwise (grupoPossui t)

  pegaCellPos :: Int -> Int -> (Int, Int)
  pegaCellPos linha col
      | linha > 9 || col > 9 || linha < 0 || col < 0 = -1
      | otherwise s 
        where
          let a = examplePuzzle !! linha !! coluna
          let b = exampleGroups !! linha !! coluna
          s = zip a b
