module Day7 where

import Puzzle ( Puzzle )
import Data.List.Split (splitOn)
import Util (sumN)

part1 :: Puzzle
part1 = show . solve id . parseInput

part2 :: Puzzle
part2 = show . solve sumN . parseInput

solve f xs = minimum [ sum diffs | x <- [min'..max'], let diffs = map (f . abs . subtract x) xs ]
    where (min',max') = (minimum xs, maximum xs)

parseInput = map read . splitOn ","