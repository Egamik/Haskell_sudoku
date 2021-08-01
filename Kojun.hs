module Kojun where

import Data.List
import Data.Maybe

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

soluciona :: [Int] -> [Int] -> Maybe [Int]
soluciona a g = headOrNothing $ solucoes a g

-- Recebe o tabuleiro e retorna todas as solucoes
solucoes :: [Int] -> [Int] -> [[Int]]
solucoes b g = solucoes' (elemIndices 0 b) b g
    where
        -- Recebe lista do indice de lugares vazios, determina quais
        -- valores sao possiveis colocar naquele lugar e ,recursivamente,
        -- encontra todas as solucoes para aquele conjunto de valores
        solucoes' :: [Int] -> [Int] -> [Int] -> [[Int]]
        solucoes' [] m _ = [m]
        solucoes' (x:xs) m g = concatMap (solucoes' xs g) candidatosTab
            where
                candidatosVal = [l | l <- reverse [1..7], valorEhPossivel m g x l]
                candidatosTab = map (\l -> insereEmLista l x m) candidatosVal

-- Recebe:
-- [Int] tabuleiro
-- [Int] vetor de grupos
--  Int   index da posicao 
--  Int   valor
-- retorna true caso possa ser usado
valorEhPossivel :: [Int] -> [Int] -> Int -> Int -> Bool
valorEhPossivel m g i v = okVizinho && okAcima && okGrupo && okMax
    where
        okVizinho = filtraCandidatosPelosValoresAdjacentes m i [v]
        okAcima = True
        okGrupo = v `elem` valoresDisponiveisGrupo m g (g!!i)
        okMax = v <= tamanhoGrupo (g !! i) g

-- Transforma matriz em lista
linearizaMatriz :: [[Int]] -> [Int]
linearizaMatriz [] = []
linearizaMatriz (h:t) = h ++ (linearizaMatriz t)

{-| Recebe duas matrizes linearizadas e faz matriz de pares
    usada para gerar uma lista de (valor, grupoID)-}
juntaMatriz :: [Int] -> [Int] -> [(Int, Int)]
juntaMatriz [] _ = []
juntaMatriz _ [] = []
juntaMatriz a b = zip a b

-- Recebe:
--  Int      linha
--  Int      coluna
-- [Int]     matriz
--  retorna valor associado a posicao
pegaPosLista :: Int -> Int -> [Int] -> Int
pegaPosLista x y m
    | x > 9 || y > 9 || x < 0 || y < 0 = -1
    | otherwise = m !! (x*10 + y)

-- Recebe:
--  Int     ID do grupo 
-- [Int]    lista de grupos
--  retorna o tamanho
tamanhoGrupo ::Ord a => a -> [a] -> Int
tamanhoGrupo _ [] = 0
tamanhoGrupo x lista = (length . filter (==x)) lista

-- Recebe:
--  Int     valor
-- [Int]    grupo
--  retorna true caso exista o valor no grupo
grupoPossui :: Int -> [Int] -> Bool
grupoPossui _ [] = False
grupoPossui a xs = length (filter (== a) xs) >= 1

-- Recebe
-- [Int]    lista dos grupos
--  Int     label do grupo
-- Retorna lista dos indices dos elementos
posicoesGrupo :: [Int] -> Int -> [Int]
posicoesGrupo grupos valor = elemIndices valor grupos

-- Recebe:
-- [Int]    tabuleiro
-- [Int]    posicoes de um grupo
-- Retorna valores associados do tabuleiro
valoresGrupo :: [Int] -> [Int] -> [Int]
valoresGrupo board groups = [board !! i | i <- groups]

-- Recebe:
--  Int     posicao inicial
-- [Int]    tabuleiro
-- Retorna posicao da celula acima
pegaIDCima :: Int -> [Int] -> Int
pegaIDCima id xs = if id `elem` [0..9] then (-1) else id-10

-- Recebe:
--  Int     posicao inicial
-- [Int]    tabuleiro
-- Retorna posicao da celula acima
pegaIDBaixo :: Int -> [Int] -> Int
pegaIDBaixo id xs = if id `elem` [90..99] then (-1) else id+10

-- Recebe tabuleiro, vetor grupos, label do grupo e retorna valores disponiveis naquele grupo
valoresDisponiveisGrupo :: [Int] -> [Int] -> Int -> [Int]
valoresDisponiveisGrupo board groups label = [0 .. 7] \\ valores
    where
        posicoes = posicoesGrupo groups label
        valores = valoresGrupo board posicoes

-- Pega a posicao do elemento a esquerda da posicao fornecida
pegaIDEsq :: Int -> [Int] -> Int
pegaIDEsq id xs = if id `mod` 10 == 0 then (-1) else id-1

-- Pega a posicao do elemento a direita da posicao fornecida
pegaIDDir :: Int -> [Int] -> Int
pegaIDDir id xs = if id `mod` 9 == 0 then (-1) else id+1

-- Recebe index da posicao, tabuleir e retorna valor dos vizinhos adjacentes
pegaVizinhos :: Int -> [Int] -> [Int]
pegaVizinhos _ [] = []
pegaVizinhos id mat = [mat !! pegaIDCima id mat, mat !! pegaIDBaixo id mat, mat !! pegaIDEsq id mat, mat !! pegaIDDir id mat]

-- Insere elemento novo no index id na lista fornecida
insereEmLista :: Int -> Int -> [Int] -> [Int]
insereEmLista novo id (h:t)
    | id <= 0 = novo:h:t
    | otherwise = h : insereEmLista novo (id-1) t

-- Filtra os valores candidatos removendo os valores adjacentes à célula.
-- Parâmetros:
---- [Int]: lista de valores do tabuleiro.
---- Int: índice da célula sendo comparada.
---- [Int]: lista de valores candidatos para a célula.
-- Retorno:
---- [Int]: lista atualizada com valores candidatos para a célula.
filtraCandidatosPelosValoresAdjacentes :: [Int] -> Int -> [Int] -> Bool
filtraCandidatosPelosValoresAdjacentes matriz id valoresCandidatos = do
    let valoresVizinhos = pegaVizinhos id matriz
    null valoresVizinhos || not (null (filter (`elem` valoresCandidatos) valoresCandidatos))

headOrNothing :: [a] -> Maybe a
headOrNothing []     = Nothing
headOrNothing (x:xs) = Just x

mostraMatriz :: [Int] -> Int -> String
mostraMatriz [] _ = ""
mostraMatriz (h:t) aux
    | aux `mod` 10 == 0 = "\n" ++ show h ++ " " ++ mostraMatriz t (aux + 1)
    | otherwise = show h ++ " " ++ mostraMatriz t (aux + 1)