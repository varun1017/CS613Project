{-# LANGUAGE RecordWildCards #-}

module Sudoku.GUI.Events (handleEvents) where

import Prelude
import Data.Maybe
import FPPrac.Events
import Sudoku
import Sudoku.Examples
import Sudoku.Strategy ( step )
import qualified Sudoku.Strategy.HiddenSingle as HS
import qualified Sudoku.Strategy.NakedSingle as NS
import qualified Sudoku.Strategy.NakedPairs as NP
import Sudoku.GUI.State
import qualified Sudoku.GUI.Button as Btn
import qualified Sudoku.GUI.Menu as Menu
import qualified Sudoku.GUI.Solver as Solver
import qualified Sudoku.GUI.Raster as Raster
import Data.Char (digitToInt)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRIO)
import System.Directory (getCurrentDirectory, getDirectoryContents)
import System.FilePath ((</>))


handleEvents :: State -> Input -> (State, [Output])
handleEvents state@(State {stage="menu",..}) (MouseUp (mx, my)) 
  | Btn.inBoundary mx my (Menu.buttons !! 0)
  = f $ restoreState state { stage = "solver", sudoku = emptySudoku 9, dim = 9 }
  where
    f s = (s, redraw s $ MouseUp (mx, my))

handleEvents state@(State {stage="solver",dim=d,sudoku=su,..}) (MouseUp (mx, my))
  | Btn.inBoundary mx my (Solver.buttons !! 0)
  = f $ restoreState state { stage = "menu" }
  | Btn.inBoundary mx my (Solver.buttons !! 1)
  = f $ restoreState state { sudoku = emptySudoku d }
  | Btn.inBoundary mx my (Solver.buttons !! 2)
  = f $ restoreState state { sudoku = ns2 }
  | Btn.inBoundary mx my (Solver.buttons !! 3)
  = f $ restoreState state { sudoku = ns }
  | Btn.inBoundary mx my (Solver.buttons !! 4)
  = f $ restoreState state { sudoku = unsafePerformIO (exampleSudokuFromFile "/home/varun/sudoku/puzzles/easy2.sud") }
-- Inside the handleEvents function
  -- Inside the handleEvents function
  -- | Btn.inBoundary mx my (Solver.buttons !! 4) = do
  --     puzzleFile <- randomPuzzleFile "./puzzles"
  --     let newState = state { sudoku = unsafePerformIO (exampleSudokuFromFile puzzleFile) }
  --         output = [GraphPrompt ("Enter a number", hint)]
  --     return output
  | isJust cell
  = (restoreState state { selectedCell = cell }, [GraphPrompt ("Enter a number", hint)])
  where
    -- range = allowedChars su
    hint = "Enter from keyboard"
    cell = Raster.calculateCell mx my d
    f s = (s, redraw s $ MouseUp (mx, my))
    ns2 = step su [NS.resolveAllCandidates, HS.resolveAllCandidates, NP.resolveAllCandidates]
    ns = solveFirstGrid su

handleEvents state@(State {stage="solver",selectedCell=sc,sudoku=su,..}) (Prompt ("Enter a number", n))
  = (s', redraw s' NoInput)
  where
    c = if null n then 0 else digitToInt (head n)
    (row, column) = fromJust sc
    su' | c /= 0 &&
          (isNothing sc ||
          not (isAllowed su row column c))
        = su
        | isJust sc
        = (update (su) (fst $ fromJust sc) (snd $ fromJust sc)) c
    s'  | su == su' && c /= 0
        = state { selectedCell = Nothing, sudoku = su', invalidCell = sc }
        | otherwise
        = state { selectedCell = Nothing, sudoku = su' }

handleEvents s (MouseMotion (mx, my))
  = (s, redraw s $ MouseMotion (mx, my))

handleEvents s (MouseDown (mx, my)) =
  let s' = s { mousePressed = True, invalidCell = Nothing }
  in (s', redraw s' $ MouseDown (mx, my))

handleEvents s (MouseUp (mx, my)) =
  let s' = s { mousePressed = False }
  in (s', redraw s' $ MouseUp (mx, my))

handleEvents s _ = (s, [])


restoreState :: State -> State
restoreState s = s { mousePressed = False }

redraw :: State -> Input -> [Output]
redraw s e
  | stage s == "menu"
  = [DrawOnBuffer True, ScreenClear, DrawPicture $ Menu.draw s e]
  | stage s == "solver"
  = [DrawOnBuffer True, ScreenClear, DrawPicture $ Solver.draw s e]

-- Function to randomly select a puzzle file from a folder
randomPuzzleFile :: FilePath -> IO FilePath
randomPuzzleFile folder = do
    files <- filter (\f -> f /= "." && f /= "..") <$> getDirectoryContents folder
    randomIndex <- randomRIO (0, length files - 1)
    let puzzleFile = folder ++ files !! randomIndex
    return puzzleFile

