
import Data.List
import Data.Maybe
import Debug.Trace

main = do
    let tab = linearizaMatriz examplePuzzle
    let group = linearizaMatriz exampleGroups
    let sol = soluciona tab group
    case sol of
        Just m -> putStrLn (mostraMatriz m 0)
        Nothing -> print "Erro"

debug = flip trace

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

-- Recebe o tabuleiro e grupos e retorna todas as solucoes
solucoes :: [Int] -> [Int] -> [[Int]]
solucoes b g = solucoes' (elemIndices 0 b) g b
    where
        -- Recebe lista do indice de lugares vazios, o tabuleiro e a lista de grupos,
        -- determina quais valores sao possiveis colocar naquele lugar e ,recursivamente,
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
-- retorna true caso possa ser usado
valoresPossiveis :: [Int] -> [Int] -> Int -> [Int]
valoresPossiveis m g i = valores
    where
        valoresGrupo = valoresDisponiveisGrupo m g (g!!i)
        valoresVizinho = filtraValoresAdjacentes m i valoresGrupo
        valores = filtraValoresPelaCelulaAcima m g i valoresVizinho


-- Transforma matriz em lista
linearizaMatriz :: [[Int]] -> [Int]
linearizaMatriz [] = []
linearizaMatriz (h:t) = h ++ (linearizaMatriz t)

-- Recebe:
--  Int     ID do grupo 
-- [Int]    lista de grupos
--  retorna o tamanho
tamanhoGrupo ::Ord a => a -> [a] -> Int
tamanhoGrupo _ [] = 0
tamanhoGrupo x lista = (length . filter (==x)) lista

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

-- Recebe
-- [Int]    tabuleiro
-- [Int]    grupos
--  Int     label do grupo
-- Retorna valores disponiveis naquele grupo
valoresDisponiveisGrupo :: [Int] -> [Int] -> Int -> [Int]
valoresDisponiveisGrupo board groups label = valores
    where
        posicoes = posicoesGrupo groups label
        valorMax = tamanhoGrupo label groups
        valoresG = valoresGrupo board posicoes
        valores = reverse ([1..valorMax] \\ valoresG)

-- Recebe:
--  Int     posicao inicial
-- [Int]    tabuleiro
-- Retorna posicao da celula acima
pegaIDCima :: Int -> Maybe Int
pegaIDCima id = if id `elem` [0..9] then Nothing else Just (id-10)

-- Recebe:
--  Int     posicao inicial
-- [Int]    tabuleiro
-- Retorna posicao da celula abaixo
pegaIDBaixo :: Int -> Maybe Int
pegaIDBaixo id = if id `elem` [90..99] then Nothing else Just (id+10)

-- Recebe:
--  Int     posicao inicial
-- [Int]    tabuleiro
-- Retorna posicao da celula a esquerda
pegaIDEsq :: Int -> Maybe Int
pegaIDEsq id = if id `mod` 10 == 0 then Nothing else Just (id-1)

-- Recebe:
--  Int     posicao inicial
-- [Int]    tabuleiro
-- Retorna posicao da celula a direita
pegaIDDir :: Int -> Maybe Int
pegaIDDir id = if (id+1) `mod` 10 == 0 then Nothing else Just (id+1)

-- Recebe:
--  Int     index da posicao
-- [Int]    tabuleiro
-- Retorna valor dos vizinhos adjacentes
pegaVizinhos :: Int -> [Int] -> Int -> [Int] -> [Int]
pegaVizinhos _ [] _ _ = []
pegaVizinhos id mat aux res = do
    let cima = pegaIDCima id
    if aux == 0 then
        case pegaIDCima id of
            Just x -> pegaVizinhos id mat (aux+1) ((mat !! x) : res); Nothing -> pegaVizinhos id mat (aux+1) res
    else if aux == 1 then
        case pegaIDBaixo id of
            Just y -> pegaVizinhos id mat (aux+1) ((mat !! y) : res); Nothing -> pegaVizinhos id mat (aux+1) res
    else if aux == 2 then
        case pegaIDDir id of
            Just z -> pegaVizinhos id mat (aux+1) ((mat !! z) : res); Nothing -> pegaVizinhos id mat (aux+1) res
    else if aux == 3 then
        case pegaIDEsq id of
            Just j -> pegaVizinhos id mat (aux+1) ((mat !! j) : res); Nothing -> pegaVizinhos id mat (aux+1) res
    else res

-- Recebe:
--  Int:    Valor a ser inserido
--  Int:    Indice a ser usado
-- [Int]:   Lista na qual quer inserir
-- Retorna lista com novo elemento
insereEmLista :: Int -> Int -> [Int] -> [Int]
insereEmLista novo _ [] = [novo]
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
filtraValoresAdjacentes :: [Int] -> Int -> [Int] -> [Int]
filtraValoresAdjacentes matriz id valoresCandidatos = res
    where
        valoresVizinhos = pegaVizinhos id matriz 0 []
        res = valoresCandidatos \\ valoresVizinhos
-- Filtra os valores candidatoos removendo o valor da celula acima
-- Recebe:
-- [Int]:   lista dos valores do tabuleiro
-- [Int]:   lista de grupos
--  Int:    indice da celula
-- [Int]:   lista de valores candidatos a serem  filtrados
filtraValoresPelaCelulaAcima :: [Int] -> [Int] -> Int -> [Int] -> [Int]
filtraValoresPelaCelulaAcima matriz grupos id candidatos = do
    case pegaIDCima id of
        Just x -> if (grupos !! x) == (grupos !! id) then candidatos \\ [(matriz !! x)..9]; else candidatos
        Nothing -> candidatos


headOrNothing :: [a] -> Maybe a
headOrNothing []     = Nothing
headOrNothing (x:xs) = Just x

-- Recebe vetor e formata de acordo com o tamanho
-- [Int]:   Matriz
--  Int:    Numero auxiliar para recursividade
-- Retorna string formatada
mostraMatriz :: [Int] -> Int -> String
mostraMatriz [] _ = ""
mostraMatriz (h:t) aux
    | aux `mod` 10 == 0 = "\n" ++ show h ++ " " ++ mostraMatriz t (aux + 1)
    | otherwise = show h ++ " " ++ mostraMatriz t (aux + 1)
