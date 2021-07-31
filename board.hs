{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module KojunBoard where

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

-- Recebe lista dos grupos, o label do grupo e retorna lista dos indices dos elementos
--posicoesGrupo :: [Int] -> Int -> [Int]
--posicoesGrupo [] _ = [0]
--posicoesGrupo xs lbl = helper 0 where
--    helper _ [] = []
--    helper i (lbl:xs) = i : helper (i+1) xs    
--    helper i (_:xs) = helper (i+1) xs