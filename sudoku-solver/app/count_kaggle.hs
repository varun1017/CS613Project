import System.Environment (getArgs)
import Data.List.Split (splitOn, chunksOf)
import Control.Monad (forM_)
import Sudoku (readSudoku, solve'', parsedGrid)
import Control.Monad.Writer
import Data.Monoid (Sum(..))
import Text.Read (readMaybe)

type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Int

-- Function to parse CSV puzzle into 9x9 format
parseCSVToSudoku :: String -> [[Int]]
parseCSVToSudoku csvPuzzle = map (map read . chunksOf 1) $ chunksOf 9 csvPuzzle



-- parseCSVToSudoku :: String -> Maybe [[Int]]
-- parseCSVToSudoku csvPuzzle = traverse (traverse readMaybe) $ chunksOf 9 (drop 1 $ lines csvPuzzle)


-- Function to print Sudoku grid
printGrid :: [[Int]] -> IO ()
printGrid grid = mapM_ (putStrLn . unwords . map show) grid

main :: IO ()
main = do
    args <- getArgs
    case args of
        [csvFilePath] -> do
            -- Read the CSV file
            contents <- readFile csvFilePath
            let puzzles = lines contents
                puzzlesWithSolutions = map (splitOn ",") puzzles
                sudokuPairs = map (\[puzzle, _] -> puzzle) puzzlesWithSolutions
            -- Iterate over each puzzle-solution pair
            forM_ sudokuPairs $ \quiz -> do
                let grid = (parseCSVToSudoku quiz)
                putStrLn "Solving puzzle:"
                -- printGrid grid
                -- Solve the puzzle
                -- (result, totalCount) <- solvePuzzle grid
                let (result, Sum totalCount) = runWriter (solve'' grid)
                -- Export the number of steps taken to solve the puzzle into a file
                let steps = totalCount
                appendFile ( "./" ++ "kaggle_steps.txt") (show steps ++ "\n")
                putStrLn $ "Steps taken to solve: " ++ show steps
        _ -> putStrLn "Usage: ./count_kaggle <csv_file_path>"

-- -- Function to solve the puzzle and count steps
-- solvePuzzle :: [[Int]] -> IO (Grid, Sum Int)
-- solvePuzzle grid = do
--     (result, steps) <- runWriterT (solve'' grid)
--     return (result, steps)