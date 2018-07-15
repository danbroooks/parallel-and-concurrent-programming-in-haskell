# Parallel and Concurrent Programming in Haskell

## Sudoku

To run the sudoku examples, run the `sudoku` executable followed by the
execution mode and puzzle file you want to run:

```
cabal new-run sudoku par 1000 -- +RTS -N2
```

To run with performance metrics, use the `-s` flag:

```
cabal new-run sudoku par 1000 -- +RTS -N2 -s
```
