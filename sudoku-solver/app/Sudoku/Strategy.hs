module Sudoku.Strategy where

import Sudoku
      ( Grid,
      columnCount,
      isAllowed',
      isTaken,
      rowCount,
      update,
      allowedChars)
import Prelude
import Data.Maybe ( fromJust, isJust, isNothing )
import Data.List ( findIndex )

type Solver = (Grid -> [[(Int, [Int])]])

findCandidates :: Grid -> Int -> Int -> [Int]
findCandidates su i j | isTaken su i j  = []
                      | otherwise       = filter ((isAllowed') su i j) (allowedChars su)

findResolvableCell :: Grid -> Solver -> Maybe (Int, Int, Int)
findResolvableCell su f
  | isNothing m = Nothing
  | isJust m = let
      m' = fromJust m
      i = div m' (rowCount su)
      j = mod m' (columnCount su)
      (_, cs) = rcs !! m'
    in
      Just (i, j, head cs)
  where
    rcs = concat (f su)
    m   = findIndex (\(_, cs) -> length cs == 1) rcs

step :: Grid -> [Solver] -> Grid
step su [] = su
step su (f:fs)
  | isNothing r = step su fs
  | isJust r = let (i, j, c) = fromJust r in update su i j c
  where
    r = findResolvableCell su f

run :: Grid -> Solver -> Grid
run su f  | outcome /= su = run outcome f
          | otherwise     = outcome
          where
            outcome = map (\r -> (map resolve r)) (f su)
            resolve (s, cs) | length cs == 1  = head cs
                            | otherwise       = s


