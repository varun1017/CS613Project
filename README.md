# Project Title

Sudoku-Solver-Classifier

## Project Description

This project aims to develop an advanced Sudoku solver that not only efficiently solves Sudoku puzzles using the AC-3 algorithm and backtracking but also enhances these techniques for better performance. A key feature of our solver is its ability to classify puzzles into different categories, enabling users to understand the complexity of a given puzzle at a glance. We refine the AC-3 algorithm, as well as incorporate additional solving strategies, to optimize our solver's efficiency and effectiveness across different puzzle difficulties.

To make our solver accessible and user-friendly, we design and implement a graphical user interface (GUI). This GUI will not only allow for puzzle input and displaying solutions but also include a HINT button to provide hints for the next cell.


## How To Install and Run

	$ git clone https://github.com/varun1017/CS613Project.git
	$ cd CS613Project
	$ cabal install --user
	$ cabal install --lib split
	$ cabal install --lib mtl
	$ cabal run

### Note

Due to large size of the kaggle dataset it was not uploaded here and the link to it is https://www.kaggle.com/datasets/bryanpark/sudoku
