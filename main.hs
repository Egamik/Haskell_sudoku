import Prelude
import Data.Tuple
import Data.List
import GameBoard

main = do
    let board = [((0, 0), 1, 1), ((0, 1), 2, 1), ((0, 2), 3, 3), ((0, 3), 4, 3), ((0, 4), 5, 3), ((0, 5), 6, 3), ((0, 6), 7, 3), ((0, 7), 8, 3), ((0, 8), 9, 3), ((0, 9), 10, 3),
                 ((1, 0), 11, 2), ((1, 1), 12, 1), ((1, 2), 13, 2), ((1, 3), 14, 3), ((1, 4), 15, 3), ((1, 5), 16, 3), ((1, 6), 17, 3), ((1, 7), 18, 3), ((1, 8), 19, 3), ((1, 9), 20, 3),
                 ((2, 0), 21, 2), ((2, 1), 22, 2), ((2, 2), 23, 2), ((2, 3), 24, 3), ((2, 4), 25, 3), ((2, 5), 26, 3), ((2, 6), 27, 3), ((2, 7), 28, 3), ((2, 8), 29, 3), ((2, 9), 30, 3),
                 ((3, 0), 31, 2), ((3, 1), 32, 2), ((3, 2), 33, 2), ((3, 3), 34, 3), ((3, 4), 35, 3), ((3, 5), 36, 3), ((3, 6), 37, 3), ((3, 7), 38, 3), ((3, 8), 39, 3), ((3, 9), 40, 3),
                 ((4, 0), 41, 2), ((4, 1), 42, 2), ((4, 2), 43, 2), ((4, 3), 44, 3), ((4, 4), 45, 3), ((4, 5), 46, 3), ((4, 6), 47, 3), ((4, 7), 48, 3), ((4, 8), 49, 3), ((4, 9), 50, 3),
                 ((5, 0), 51, 2), ((5, 1), 52, 2), ((5, 2), 53, 2), ((5, 3), 54, 3), ((5, 4), 55, 3), ((5, 5), 56, 3), ((5, 6), 57, 3), ((5, 7), 58, 3), ((5, 8), 59, 3), ((5, 9), 60, 3),
                 ((6, 0), 61, 2), ((6, 1), 62, 2), ((6, 2), 63, 2), ((6, 3), 64, 3), ((6, 4), 65, 3), ((6, 5), 66, 3), ((6, 6), 67, 3), ((6, 7), 68, 3), ((6, 8), 69, 3), ((6, 9), 70, 3),
                 ((7, 0), 71, 2), ((7, 1), 72, 2), ((7, 2), 73, 2), ((7, 3), 74, 3), ((7, 4), 75, 3), ((7, 5), 76, 3), ((7, 6), 77, 3), ((7, 7), 78, 3), ((7, 8), 79, 3), ((7, 9), 80, 3),
                 ((8, 0), 81, 3), ((8, 1), 82, 2), ((8, 2), 83, 2), ((8, 3), 84, 3), ((8, 4), 85, 3), ((8, 5), 86, 3), ((8, 6), 87, 3), ((8, 7), 88, 3), ((8, 8), 89, 3), ((8, 9), 90, 3),
                 ((9, 0), 91, 2), ((9, 1), 92, 2), ((9, 2), 93, 2), ((9, 3), 94, 3), ((9, 4), 95, 3), ((9, 5), 96, 3), ((9, 6), 97, 3), ((9, 7), 98, 3), ((9, 8), 99, 3), ((9, 9), 100, 3)]
    
    printGameBoard board 0 10

    print "============================================================"

    let cell = ((9, 0), 91, 2)
    printCell cell
    print (isValidSolutionCell board cell)

    let position = Just (1, 0)
    let position2 = Just (5, 3)
    let cell1 = getCell board position
    let cell2 = getCell board position2
    case cell1 of
        Just a -> do
            print a
            case getAdjacentCell board getTopPosition a of
                Just x -> print ("Top cell: " ++ show (getCellValue x))
                Nothing -> print "No cells on top."
            case getAdjacentCell board getBottomPosition a of
                Just x -> print ("Bottom cell: " ++ show (getCellValue x))
                Nothing -> print "No cells at bottom."
            case getAdjacentCell board getLeftPosition a of
                Just x -> print ("Left cell: " ++ show (getCellValue x))
                Nothing -> print "No cells at left."
            case getAdjacentCell board getRightPosition a of
                Just x -> print ("Right cell: " ++ show (getCellValue x))
                Nothing -> print "No cells at right."
            case cell2 of
                Just b -> print (isInSameGroup a b)
                Nothing -> print "Nothing"
        Nothing -> print "Nothing"