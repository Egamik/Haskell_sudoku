{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module KojunBoard where

import Data.List
import Data.Maybe
import Data.Monoid ((<>))

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

{-| Recebe duas matrizes linearizadas e faz matriz de pares
    usada para gerar uma lista de (valor, grupoID)-}
juntaMatriz :: [Int] -> [Int] -> [(Int, Int)]
juntaMatriz [] _ = []
juntaMatriz _ [] = []
juntaMatriz a b = zip a b

-- Recebe linha, coluna, vetor e retorna valor associado a posicao
pegaPosLista :: Int -> Int -> [Int] -> Int
pegaPosLista x y m
    | x > 9 || y > 9 || x < 0 || y < 0 = -1
    | otherwise = m !! (x*10 + y)

-- Recebe ID do grupo e a lista de grupos e retorna o tamanho
tamanhoGrupo ::Ord a => a -> [a] -> Int
tamanhoGrupo _ [] = 0
tamanhoGrupo x lista = (length . filter (==x)) lista

-- Recebe um numero, um grupo e retorna true caso exista o valor no grupo
grupoPossui :: Int -> [Int] -> Bool
grupoPossui _ [] = False
grupoPossui a xs = length (filter (== a) xs) == 1

-- Recebe matriz em lista dos grupos, o label do grupo e retorna lista dos indices dos elementos
posicoesGrupo :: [Int] -> Int -> [Int]
posicoesGrupo grupos valor = elemIndices valor grupos

-- Recebe tabuleiro, posicoes de um grupo e retorna valores associados do tabuleiro
valoresGrupo :: [Int] -> [Int] -> [Int]
valoresGrupo board groups = [board !! i | i <- groups]

-- Pega a posicao do elemento acima da posicao fornecida
pegaIDCima :: Int -> [Int] -> Int
pegaIDCima id xs = if id `elem` [0..9] then (-1) else id-10

-- Pega a posicao do elemento abaixo da posicao fornecida
pegaIDBaixo :: Int -> [Int] -> Int
pegaIDBaixo id xs = if id `elem` [90..99] then (-1) else id+10

-- Pega a posicao do elemento a esquerda da posicao fornecida
pegaIDEsq :: Int -> [Int] -> Int
pegaIDEsq id xs = if id `mod` 10 == 0 then (-1) else id-1

-- Pega a posicao do elemento a direita da posicao fornecida
pegaIDDir :: Int -> [Int] -> Int
pegaIDDir id xs = if id `mod` 9 == 0 then (-1) else id+1

pegaVizinhos :: Int -> [Int] -> [Int]
pegaVizinhos _ [] = []
pegaVizinhos id mat = [mat !! pegaIDCima id mat, mat !! pegaIDBaixo id mat, mat !! pegaIDEsq id mat, mat !! pegaIDDir id mat]

-- Insere elemento novo no index id na lista fornecida
insereEmLista :: Int -> Int -> [Int] -> [Int]
insereEmLista novo id (h:t)
    | id <= 0 = novo:h:t
    | otherwise = h : insereEmLista novo (id-1) t

-- Filtra os valores possíveis para serem menores que o valor da célula de cima, 
-- se ela pertencer ao mesmo grupo.
-- Parâmetros:
---- [(Int, Int)]: lista de pares (valor, grupoID).
---- [Int]: lista de valores do tabuleiro.
---- Int: índice da célular sendo comparada.
---- [Int]: lista de valores candidatos para a célula.
-- Retorno:
---- [Int]: lista atualizada com valores candidatos para a célula.
menoresQueCelulaDeCima :: [(Int, Int)] -> [Int] -> Int -> [Int] -> [Int]
menoresQueCelulaDeCima pares board id valores = do
    let idCima = pegaIDCima id board
    if idCima == -1 || snd (pares !! id) /= snd (pares !! idCima)
        then valores
    else filter (< (board !! idCima)) valores