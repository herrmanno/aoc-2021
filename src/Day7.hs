module Day7 where

import Puzzle ( Puzzle )
import Data.List.Split (splitOn)
import Util (sumN)

part1 :: Puzzle Int
part1 = solve id . parseInput

part2 :: Puzzle Int
part2 = solve sumN . parseInput

solve f xs = minimum [ sum diffs | x <- [min'..max'], let diffs = map (f . abs . subtract x) xs ]
    where (min',max') = (minimum xs, maximum xs)

parseInput = map read . splitOn ","