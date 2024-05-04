module Sudoku.Strategy.NakedSingle where

import Data.Char
import Data.List
import Prelude
import Sudoku
import Sudoku.Strategy

resolveAllCandidates :: Grid -> [[(Int, [Int])]]
resolveAllCandidates su = mapWithIndeces su (\i j -> (su !! i !! j, findCandidates su i j))

