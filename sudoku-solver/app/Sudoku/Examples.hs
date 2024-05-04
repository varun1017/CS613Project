module Sudoku.Examples (emptySudoku, exampleSudoku, exampleSudokuFromFile) where

import Sudoku ( Grid, readSudoku )
import Prelude


exampleSudoku :: Int -> Grid

exampleSudoku 9 =
  [ [4,0,0,0,5,3,0,0,0]
  , [0,0,0,2,9,4,0,7,3]
  , [3,0,0,0,7,0,5,0,9]
  , [7,8,0,0,0,0,0,1,0]
  , [0,0,0,0,0,0,0,0,0]
  , [0,4,0,0,0,0,0,2,5]
  , [2,0,8,0,1,0,0,0,7]
  , [9,1,0,7,6,2,0,0,0]
  , [0,0,0,8,3,0,0,0,2] ]

emptySudoku :: Int -> Grid
emptySudoku d = replicate d (replicate d 0)


-- | Read Sudoku from a file and return it as a grid.
exampleSudokuFromFile :: FilePath -> IO Grid
exampleSudokuFromFile filePath = readSudoku filePath


