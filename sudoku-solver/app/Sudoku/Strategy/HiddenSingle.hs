module Sudoku.Strategy.HiddenSingle where

import Prelude
import Sudoku
    ( Grid,
      rowCount,
      columnCount,
      blockHeight,
      blockWidth,
      mapWithIndeces )
import Sudoku.Strategy
import Data.List

concatRowCandidates :: Grid -> Int -> [Int]
concatRowCandidates su i = concat [ findCandidates su i j | j <- [0..(columnCount su - 1)] ]

concatColumnCandidates :: Grid -> Int -> [Int]
concatColumnCandidates su j = concat [ findCandidates su i j | i <- [0..(rowCount su - 1)] ]

concatBlockCandidates :: Grid -> Int -> Int -> [Int]
concatBlockCandidates su i j
  = concat [ findCandidates su k l | k <- [i'..i''], l <- [j'..j''] ]
  where
    h   = blockHeight su
    w   = blockWidth su
    i'  = (div i h) * h
    j'  = (div j w) * w
    i'' = i' + h - 1
    j'' = j' + w - 1

findUniqueRowCandidates :: Grid -> Int -> Int -> [Int]
findUniqueRowCandidates su i _ = concat $ filter (\x -> length x == 1) $ group $ sort $ concatRowCandidates su i

findUniqueColumnCandidates :: Grid -> Int -> Int -> [Int]
findUniqueColumnCandidates su _ j = concat $ filter (\x -> length x == 1) $ group $ sort $ concatColumnCandidates su j

findUniqueBlockCandidates :: Grid -> Int -> Int -> [Int]
findUniqueBlockCandidates su i j = concat $ filter (\x -> length x == 1) $ group $ sort $ concatBlockCandidates su i j

resolveCandidates :: Grid -> Int -> Int -> (Int, [Int])
resolveCandidates su i j  
  | not (null urci) = (s, urci)
  | not (null ucci) = (s, ucci)
  | not (null ubci) = (s, ubci)
  | otherwise       = (s, cs)
  where
    s = su !! i !! j
    cs = findCandidates su i j
    urci = intersect cs (findUniqueRowCandidates su i j)
    ucci = intersect cs (findUniqueColumnCandidates su i j)
    ubci = intersect cs (findUniqueBlockCandidates su i j)


resolveAllCandidates :: Grid -> [[(Int, [Int])]]
resolveAllCandidates su = mapWithIndeces su (\i j -> resolveCandidates su i j)

solve :: Grid -> Grid
solve su = run su resolveAllCandidates

