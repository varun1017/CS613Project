-- Export the functions to let main.hs use
module Sudoku (readSudoku, printGrid, solve, printSolutions, Grid, isValid, blockWidth, blockHeight, solveFirstGrid, isAllowed, update, isAllowed', isTaken, rowCount, columnCount, allowedChars, mapWithIndeces, findBlock, solve', solve'', parsedGrid) where

import Data.List (transpose, nub, (\\), minimumBy)
import Data.Char (digitToInt, isDigit)
import Prelude
import Debug.Trace
import Control.Monad.Writer
import Data.Monoid

type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Int
-- Choices represents the domain for every cell (primary variable) in AC-3 algorithm
type Choices = [Value]
-- Constraint tells which two cells does this constraint related to using the cell position
type Constraint = ((Int, Int), (Int, Int))

boxsize :: Int
boxsize = 3

values :: [Value]
values = [1..9]

-- Extracting rows
rows :: Matrix a -> [Row a]
rows = id

-- Extracting columns
-- Example: cols [[1,2],  = [[1,3],
--                [3,4]]     [2,4]]
cols :: Matrix a -> [Row a]
cols = transpose

-- Extracting boxs
-- Example: if boxsize = 2, then we have
-- [[1,2,3,4],                 [[[[1,2],[3,4]],                         [[[[1,2],[5,6]],                        [[1,2,5,6],
--  [5,6,7,8],       --pack->    [[5,6],[7,8]]],        --map cols->      [[3,4],[7,8]]],       --unpack->       [3,4,7,8],
--  [9,10,11,12],               [[[9,10],[11,12]],                       [[[9,10],[13,14]],                      [9,10,13,14],
--  [13,14,15,16]]               [[13,14],[15,16]]]]                      [[11,12],[15,16]]]]                    [11,12,15,16]]
boxs :: Matrix a -> [Row a]
boxs = unpack . map cols . pack
    where
        pack   = split . map split
        split  = chop boxsize
        unpack = map concat . concat

chop :: Int -> [a] -> [[a]]
chop _ [] =  []
chop n xs =  take n xs : chop n (drop n xs)

-- A grid is valid if there are no duplicates in any row, column or box
valid :: Grid -> Bool
valid g = all nodups (rows g) && all nodups (cols g) && all nodups (boxs g)

nodups :: Eq a => [a] -> Bool
nodups []     = True
nodups (x:xs) = notElem x xs && nodups xs

-- Solve a Sudoku puzzle using backtracking combined with AC-3 algorithm
solve :: Grid -> [Grid]
solve = search . reduceDomains . choices

-- Solve a Sudoku puzzle using backtracking combined with AC-3 algorithm
solve' :: Grid -> [(Grid, Int)]
solve' = search' 0. reduceDomains . choices

solve'' :: Grid -> Writer (Sum Int) [(Grid, Int)]
solve'' = search'' 0. reduceDomains . choices

solveFirstGrid :: Grid -> Grid
solveFirstGrid grid = head (solve grid)

-- Replaces 0 in a grid by all possible values for that square, giving a matrix of choices
choices :: Grid -> Matrix Choices
choices = map (map choice) 
    where
        choice c | c == 0  =  [1..9]
                 | otherwise =  [c]

-- Search for solutions using backtracking
    -- If the grid is blocked, then there is no possible solution. Return an empty list
    -- If all cells have only one choice, collapse the matrix to convert Matrix Choices into Grid
    -- Using expand to take next step and implement AC-3 algorithm (reduceDomains) to recursively search for solution
search :: Matrix Choices -> [Grid]
search m | blocked m = []
         | all (all single) m = collapse m
        --  | otherwise = 
        --      [g | m' <- expandLeastChoices m, g <- search (reduceDomains m')]
         | otherwise = do
            trace ("Intermediate state:\n" ++ showChoices m) [g | m' <- expand m, g <- search (reduceDomains m')]

search' :: Int -> Matrix Choices -> [(Grid, Int)]
search' count m
    | blocked m = [([], count)]
    | all (all single) m = [(m', count)| m' <- collapse m, safe' m']
    -- | otherwise = concat [search' (count + 1) (reduceDomains m') | m' <- expandLeastChoices m, not (blocked m')]
    | otherwise = [(result, count') | m' <- expandLeastChoices m
                                   , not (blocked m')
                                   , (result, count') <- search' (count + 1) (reduceDomains m')]

-- This function performs a search for solutions to a Sudoku puzzle using backtracking.
-- It explores the solution space by recursively trying different choices until a solution is found or all possibilities are exhausted.
search'' :: Int -> Matrix Choices -> Writer (Sum Int) [(Grid, Int)]
search'' count m
    -- If the puzzle is blocked (i.e., there is a contradiction in the choices), 
    -- return the current state with a count of 1, indicating one exploration step.
    | blocked m = tell (Sum 1) >> return [([], count)]
    -- If all cells have only one possible choice, and the puzzle is safe, 
    -- return the solved puzzle with the current count, indicating one exploration step.
    | all (all single) m = tell (Sum 1) >> return [(m', count) | m' <- collapse m, safe' m']
    -- If neither of the above cases apply, continue the search recursively.
    | otherwise = do
        -- Increment the count to track the number of exploration steps.
        tell 1
        -- Explore all possible choices for the next cell with the fewest options.
        results <- sequence [search'' (count + 1) (reduceDomains m') | m' <- expandLeastChoices m, not (blocked m')]
        -- Check if any of the results contain a non-empty solution.
        let nonEmptyResults = takeUntil (\(res, _) -> null res) (concat results)
        -- If there are non-empty results, return them; otherwise, continue searching.
        case nonEmptyResults of
            [] -> return $ concat results
            _  -> return nonEmptyResults


-- Define takeUntil
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs)
    | p x = x : takeUntil p xs
    | otherwise = [x]

-- Function to convert matrix of choices to a string representation
showChoices :: Matrix Choices -> String
showChoices = unlines . map (unwords . map showChoice)
    where
        showChoice :: [Int] -> String
        showChoice [x] = show x
        showChoice xs = show xs            

-- A sudoku choice matrix is blocked if any cell has no choice or any two cells in a row/cols/box are inconsistent
blocked :: Matrix Choices -> Bool
blocked m = void m || not (safe m)
    where void m = any (any null) m

safe :: Matrix Choices -> Bool
safe m = all consistent (rows m) && all consistent (cols m) && all consistent (boxs m)

-- If two cells in a given Row Choices have same single choice, then they are inconsistent. 
-- Since it is impossible to find a solution.
consistent :: Row Choices -> Bool
consistent row = not (anyDuplicates singles)
    where
        singles = [x | [x] <- row] 
        anyDuplicates [] = False
        anyDuplicates (x:xs) = elem x xs || anyDuplicates xs 

-- The blocked function checks if any cell has no choice or if any two cells in a row/cols/box are inconsistent
blocked' :: Matrix Value -> Bool
blocked' m = not (safe' m)
    where void m = any (any isBlank) m
          isBlank = (== ' ')

-- The safe function checks if all rows, columns, and boxes in the Sudoku grid are consistent
safe' :: Matrix Value -> Bool
safe' m = all consistent' (rows m) && all consistent' (cols m) && all consistent' (boxs m)

-- The consistent function checks if a row of choices is consistent
consistent' :: Row Value -> Bool
consistent' row = length row == length (nub row)
    -- where singles = [x | x <- row]  -- Filter out blank spaces
    --       anyDuplicates [] = False
    --       anyDuplicates (x:xs) = elem x xs || anyDuplicates xs 

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates xs = length xs /= length (nub xs)

-- Reducing a matrix of choices to a choice of matrices can be defined 
-- in terms of the normal cartesian product of a list of lists, which
-- generalises the cartesian product of two lists.
-- For example, cp [[1,2],[3,4],[5,6]] gives:
--    [[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]]
cartesianProduct :: [[a]] -> [[a]]
cartesianProduct [] = [[]]
cartesianProduct (xs : xss) = [y : ys | y <- xs, ys <- cartesianProduct xss]

-- To collapse a matrix of choices
collapse :: Matrix [a] -> [Matrix a]
collapse m = cartesianProduct (map cartesianProduct m)

-- expand behaves in the same way as collapse, except that
-- it only collapses the first cell with more than one choice:
expand :: Matrix Choices -> [Matrix Choices]
expand m = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
   where
      (rows1,row:rows2) = break (any (not . single)) m
      (row1,cs:row2)    = break (not . single) row

-- Expand the matrix based on the cell with the least choices
expandLeastChoices :: Matrix Choices -> [Matrix Choices]
expandLeastChoices m = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
   where
      (rows1, row:rows2) = splitAt rowIndex m
      (row1, cs:row2) = splitAt colIndex row
      (rowIndex, colIndex) = findCellWithLeastChoices m

-- Find the cell with the least choices (excluding singletons)
findCellWithLeastChoices :: Matrix Choices -> (Int, Int)
findCellWithLeastChoices m = (rowIndex, colIndex)
  where
    (rowIndex, colIndex, _) = minimumBy (\(_, _, cs1) (_, _, cs2) -> compare (length cs1) (length cs2)) nonSingletonCells
    nonSingletonCells = filter (\(_, _, cs) -> length cs > 1) [(i, j, m !! i !! j) | i <- [0..8], j <- [0..8]]      

-- Check for a single element list
single :: [Int] -> Bool
single [_] = True
single (x:xs) = False

-- Implement AC-3 algorithm
reduceDomains :: Matrix Choices -> Matrix Choices
reduceDomains grid = reduceDomains' grid initialConstraints
    where initialConstraints = generateConstraints grid

-- Generate all the constraints
-- Example: The constraints related to (0, 0) are
-- [((0,0),(0,1)),((0,0),(0,2)),..,((0,0),(0,8)),((0,0),(1,0)),((0,0),(2,0)),((0,0),(8,0)),((0,0),(1,1)),((0,0),(1,2)),((0,0),(2,1)),((0,0),(2,2))]
generateConstraints :: Matrix Choices -> [Constraint]
generateConstraints grid = concatMap rowConstraints [0..8]
    where
        rowConstraints x = concatMap (cellConstraints x) [0..8]
        
        cellConstraints x y =
            [ ((x, y), (x', y')) | x' <- [0..8], y' <- [0..8], (x, y) /= (x', y'), inSameGroup (x, y) (x', y') ]
        
        inSameGroup (x1, y1) (x2, y2) =
            x1 == x2 || y1 == y2 || (x1 `div` 3 == x2 `div` 3 && y1 `div` 3 == y2 `div` 3)

-- If no more constraint need to apply, then return grid
-- Otherwise pick one constraint and apply it, then check if the domains has been changed.
--      If the domain doesn't change, then no need to add back constraint
--      If the domain does change, then using filterArcs to find the constraint that need to add back
-- Recursively apply the constraint until no more constraint need to be applied
reduceDomains' :: Matrix Choices -> [Constraint] -> Matrix Choices
reduceDomains' grid [] = grid
reduceDomains' grid (constraint:constraints) =
    reduceDomains' newGrid updatedConstraints
    where 
        (newConstraints, changed) = filterArcs constraint constraints originalGrid
        updatedConstraints = if changed then constraints ++ newConstraints else constraints
        newGrid = applyConstraint grid constraint
        originalGrid = grid

-- Find the constraint that need to add back if the domain changed 
filterArcs :: Constraint -> [Constraint] -> Matrix Choices -> ([Constraint], Bool)
filterArcs ((x1, y1), (x2, y2)) existingConstraints grid =
    (filteredConstraints, changed)
    where
        cellConstraints = generateCellConstraints (x1, y1)
        filteredConstraints = filter (\c -> c `notElem` (existingConstraints ++ [((x1, y1), (x2, y2))])) cellConstraints
        changed = removeInconsistentValues (x1, y1) (x2, y2) grid

-- Generate all contraints related to (x1, y1)
generateCellConstraints :: (Int, Int) -> [Constraint]
generateCellConstraints (x1, y1) =
    [((x1, y1), (x, y)) | x <- [0..8], y <- [0..8], (x1, y1) /= (x, y), inSameGroup (x1, y1) (x, y)]
    where
        inSameGroup (a, b) (c, d) = a == c || b == d || (a `div` 3 == c `div` 3 && b `div` 3 == d `div` 3)

-- Check whether the grid[x1][y1] domain change if apply the constraint
removeInconsistentValues :: (Int, Int) -> (Int, Int) -> Matrix Choices -> Bool
removeInconsistentValues (x1, y1) (x2, y2) grid =
    if length updatedChoices == length (grid !! x1 !! y1) then False else True
    where updatedChoices = if length (grid !! x2 !! y2) == 1
                           then grid !! x1 !! y1 \\ grid !! x2 !! y2
                           else grid !! x1 !! y1

-- Apply a constraint to reduce the domain of choices
-- If grid[x2][y2] has only one choice, then we can eliminate this value in grid[x1][y1] domain
applyConstraint :: Matrix Choices -> Constraint -> Matrix Choices
applyConstraint grid ((x1, y1), (x2, y2)) = updatedGrid
    where updatedGrid = if length (grid !! x2 !! y2) == 1 
                        then (take x1 grid) ++ [(take y1 row1 ++ [choices1'] ++ drop (y1 + 1) row1)] ++ (drop (x1 + 1) grid)
                        else grid
          row1 = grid !! x1
          choices1' = grid !! x1 !! y1 \\ grid !! x2 !! y2


-- Define a function to read a Sudoku puzzle from a file
readSudoku :: FilePath -> IO Grid
readSudoku filePath = do
    contents <- readFile filePath
    let grid = parsedGrid contents
    return grid

-- Parse a Sudoku grid from a string
parsedGrid :: String -> Grid
parsedGrid contents = map (map convertCell) (lines contents)
    where convertCell '.' = 0 -- Convert dot to '0' for empty cell
          convertCell c   = digitToInt c   -- Keep other characters as is


-- Example of parsing digits from the line
-- You can adjust this function based on your needs
parseDigit :: Char -> Char
parseDigit '.' = '0'
parseDigit x = x

-- Print a Sudoku grid
printGrid :: Grid -> IO ()
printGrid grid = putStrLn (formatGrid (map (map show) grid))

-- Format a Sudoku grid for printing
formatGrid :: Matrix String -> String
formatGrid grid = unlines (map unwords (addRowBorders (addColumnBorders grid)))

-- Add vertical borders to the Sudoku grid
addColumnBorders :: Matrix String -> Matrix String
addColumnBorders grid = map (\row -> interleaveVerticalBars row 3) grid
    where
        interleaveVerticalBars [] _ = [[]]
        interleaveVerticalBars row n = take n row ++ "|" : interleaveVerticalBars (drop n row) n

-- Add horizontal borders to the Sudoku grid
addRowBorders :: Matrix String -> Matrix String
addRowBorders grid = insertAt 3 dashedRow (insertAt 6 dashedRow grid)
  where
    dashedRow = replicate (length (head grid) - 1)  "-"

-- Function to insert an element at a specific position in a list
insertAt :: Int -> Row String -> Matrix String -> Matrix String
insertAt n xs xss = let (ys, zs) = splitAt n xss in ys ++ [xs] ++ zs

-- Define a function to print solution
printSolutions :: [Grid] -> IO ()
printSolutions [] = putStrLn "Complete printing solution."
printSolutions (solution:solutions) = do
    putStrLn "Solution:"
    printGrid solution
    printSolutions solutions

-- type Sudoku = [[Int]]

rowCount :: Grid -> Int
rowCount su = length su

columnCount :: Grid -> Int
columnCount su = length $ head su

isTaken :: Grid -> Int -> Int -> Bool
isTaken su i j = (su !! i !! j) /= 0

isInRow :: Grid -> Int -> Int -> Bool
isInRow su i s = elem s (su !! i)

blockHeight :: Grid -> Int
blockHeight su = floor $ sqrt $ fromIntegral $ rowCount su

blockWidth :: Grid -> Int
blockWidth su = ceiling $ sqrt $ fromIntegral $ columnCount su

findBlock :: Grid -> Int -> Int -> [[Int]]
findBlock su i j  = map ((take w) . (drop j')) (take h $ drop i' su)
                  where
                    h  = blockHeight su
                    w  = blockWidth su
                    i' = (div i h) * h
                    j' = (div j w) * w

isInBlock :: Grid -> Int -> Int -> Int -> Bool
isInBlock su i j c  = let block = concat $ findBlock su i j in
                      elem c block

isAllowed :: Grid -> Int -> Int -> Int -> Bool
isAllowed su i j c  = not (isInRow su i c) &&
                      not (isInRow (transpose su) j c) &&
                      not (isInBlock su i j c)

isAllowed' :: Grid -> Int -> Int -> Int -> Bool
isAllowed' su i j c = isAllowed su i j c -- && not (isTaken su i j)

isValid :: Grid -> Bool
isValid su  = all (==True) $ concat (mapWithIndeces su f)
            where
              su' i j = update su i j 0
              f i j   = isAllowed' (su' i j) i j (su !! i !! j)

update :: Grid -> Int -> Int -> Int -> Grid
update su i j c = mapWithIndeces su f
                where f i' j' | i' == i && j == j' = c
                              | otherwise = su !! i' !! j'

mapWithIndeces :: Grid -> (Int -> Int -> b) -> [[b]]
mapWithIndeces su f = let (r, c) = (rowCount su - 1, columnCount su - 1) in
                      map (\i -> (map (f i) [0..c])) [0..r]


allowedChars :: Grid -> [Int]
allowedChars _ = [1..9]