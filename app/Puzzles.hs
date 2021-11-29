module Puzzles (getPuzzle) where

import Puzzle (Puzzle)
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24
import qualified Day25

-- |All the puzzles for the whole event
puzzles :: [(Puzzle, Puzzle)]
puzzles =
        [ (Day1.part1, Day1.part2)
        , (Day2.part1, Day2.part2)
        , (Day3.part1, Day3.part2)
        , (Day4.part1, Day4.part2)
        , (Day5.part1, Day5.part2)
        , (Day6.part1, Day6.part2)
        , (Day7.part1, Day7.part2)
        , (Day8.part1, Day8.part2)
        , (Day9.part1, Day9.part2)
        , (Day10.part1, Day10.part2)
        , (Day11.part1, Day11.part2)
        , (Day12.part1, Day12.part2)
        , (Day13.part1, Day13.part2)
        , (Day14.part1, Day14.part2)
        , (Day15.part1, Day15.part2)
        , (Day16.part1, Day16.part2)
        , (Day17.part1, Day17.part2)
        , (Day18.part1, Day18.part2)
        , (Day19.part1, Day19.part2)
        , (Day20.part1, Day20.part2)
        , (Day21.part1, Day21.part2)
        , (Day22.part1, Day22.part2)
        , (Day23.part1, Day23.part2)
        , (Day24.part1, Day24.part2)
        , (Day25.part1, Day25.part2)
        ]

getPuzzle :: Int -> Maybe (Puzzle, Puzzle)
getPuzzle i
    | i < 1 = Nothing
    | i > length puzzles = Nothing
    | otherwise = Just $ puzzles !! (i - 1)
