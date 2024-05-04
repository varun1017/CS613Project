import System.Environment (getArgs)
import System.Directory (listDirectory)
import Data.List (isSuffixOf)
import Control.Monad (forM_)
import Sudoku (readSudoku, solve'')
import Control.Monad.Writer

main :: IO ()
main = do
    args <- getArgs
    case args of
        [folderPath] -> do
            -- Get list of files in the specified folder
            files <- listDirectory folderPath
            let sudokuFiles = filter (".sud" `isSuffixOf`) files
            -- Iterate over each file
            forM_ sudokuFiles $ \file -> do
                -- Read the Sudoku puzzle from the file
                let filePath = folderPath ++ "/" ++ file
                grid <- readSudoku filePath
                putStrLn $ "Solving puzzle in file: " ++ filePath
                -- Solve the puzzle
                let (result, Sum totalCount) = runWriter (solve'' grid)
                -- Export the number of steps taken to solve the puzzle into a file
                let steps = totalCount
                appendFile (folderPath ++ "/" ++ "steps.txt") (show steps ++ "\n")
                putStrLn $ "Steps taken to solve: " ++ show steps
        _ -> putStrLn "Usage: ./main <folder_path>"
