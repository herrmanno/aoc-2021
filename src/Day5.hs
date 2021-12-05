module Day5 where

import Puzzle ( Puzzle )
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Control.Arrow ((***))
import Util (upsert, range)

part1 :: Puzzle
part1 = show . solve (filter (not . isDiagonal))

part2 :: Puzzle
part2 = show . solve id

solve :: ([Line] -> [Line]) -> String -> Int
solve f = length
      . filter (>1)
      . M.elems
      . foldr markLine M.empty
      . f
      . parseInput

type Line = (Point,Point)
type Point = (Int,Int)

isDiagonal :: Line -> Bool
isDiagonal ((x1,y1),(x2,y2)) = x1 /= x2 && y1 /= y2

lineToPoints :: Line -> [Point]
lineToPoints line@((x1,y1),(x2,y2))
    | isDiagonal line = take len (iterate ((+xDiff) *** (+yDiff)) (x1,y1))
    | otherwise = [ (x,y) | x <- range x1 x2, y <- range y1 y2]
    where
        len = abs (x1 - x2) + 1
        xDiff = signum $ x2 - x1
        yDiff = signum $ y2 - y1

markLine :: Line -> M.Map (Int,Int) Int -> M.Map (Int,Int) Int
markLine line m = foldr (upsert 1 succ) m (lineToPoints line)

parseInput :: String -> [Line]
parseInput = fmap parseLine . lines
    where
        parseLine (words -> [a,_,b]) = (parts a, parts b)
        parts (splitOn "," -> [x,y])= (read x, read y)