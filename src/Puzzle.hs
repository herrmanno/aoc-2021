module Puzzle where

type Puzzle = String -> String

runPuzzle :: Puzzle -> String -> IO ()
runPuzzle p = putStrLn . p
