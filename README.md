# TensSolver
This project has goal to solve the [Tents puzzle](https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/tents.html) in Scheme.
Note: Aimed puzzles are less than 10x10 and more tricky puzzles mean demands more time.

## Getting Started
The project uses Scheme. You can download [Dr. Racket](https://racket-lang.org/) or add Scheme Extension to your VS Code.

## Usage
The main function is TENTS-SOLUTION. It takes a list as an argument. This list includes 3 other lists.
First list is tent counts of rows.
Second list is tent counts of columns.
Third list consists of tree coordinate lists.

Argument Looks like:
```
( (<tent-count-row1> ... <tent-count-rowN>) 
(<tent-count-column1> ... <tent-count-columnM>) 
( (<tree1-x> <tree1-y>) ... (<treeT-x> <treeT-y>) ) )
```

simple TENTS-SOLUTION example:
```
(TENTS-SOLUTION '((1 0 1) (0 2 0 0 0) ((2 2) (1 3)) ) )
```

if the solution doesn't exist then TENTS-SOLUTION returns #f (false)

## Author

* **Onur Sefa Özçıbık** - [Gepsmin](https://github.com/Gepsmin)