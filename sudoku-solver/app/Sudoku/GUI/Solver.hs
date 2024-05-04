module Sudoku.GUI.Solver (draw, buttons) where

import Prelude
import FPPrac.Events (Input)
import FPPrac.Graphics hiding (dim)
import qualified FPPrac.Graphics as Gfx (dim)
import Sudoku.GUI.State
import qualified Sudoku.GUI.Button as Btn
import qualified Sudoku.GUI.Raster as Raster
import FPPrac.Graphics (black)

buttons :: [Btn.Button]
buttons =
  [ Btn.Rectangular (290, (-160), 190, 60) "Home" (141, 21) (greyN 0.5)
  , Btn.Rectangular (290, (-240), 190, 60) "Clear All" (100, 21) (Gfx.dim red)
  , Btn.Rectangular (290, (0), 190, 60) "Hint" (100, 21) (Gfx.dim blue)
  , Btn.Rectangular (290, (240), 190, 60) "Solve" (127, 21) (Gfx.dim green)
  , Btn.Rectangular (290, (160), 190, 60) "Load Sample" (160, 21) (greyN 0.5)
  ]

draw :: State -> Input -> Picture
draw s e = Pictures
  [ drawBackground
  , Raster.draw s e
  , Btn.drawAll s e buttons
  ]

drawBackground :: Picture
drawBackground = Pictures
  [ Translate 0 0 $ Color (makeColor 0.1 0.1 0.1 1) $ rectangleSolid 800 800
  ]
