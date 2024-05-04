-- module Main where

-- -- import qualified MyLib (someFunc)

-- main :: IO ()
-- main = do
--   putStrLn "Hello, Haskell!"
--   -- MyLib.someFunc

module Main where

import Prelude (IO)
import qualified Sudoku.GUI as GUI (main)

main :: IO ()
main = GUI.main
