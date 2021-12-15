module Day15 where

import Puzzle ( Puzzle )
import Util.Map2d (to2dMap, Map2d, Coord2d, bottomRight, neighbours4, size)
import Data.Char (digitToInt)
import Data.Heap (Heap, MinPolicy)
import qualified Data.Heap as H
import Util.BFS (bfs)
import Control.Arrow (first)
import Data.Tuple (swap)
import qualified Data.Map as M

part1 :: Puzzle Cost
part1 = maybe 0 fst . findMinCostPath . parseInput

part2 :: Puzzle Cost
part2 = maybe 0 fst . findMinCostPath . enlargeInputBy (5,5) . parseInput

enlargeInputBy :: (Int, Int) -> Map2d Cost -> Map2d Cost
enlargeInputBy (mX, mY) m = m' where
    m' = to2dMap [ [f y x | x <- [0..(lenX * mX) - 1]] | y <- [0..(lenY * mY) - 1] ]
    (lenX, lenY) = size m
    f y x = case M.lookup (y,x) m of
        Just n -> n
        Nothing
            | x - lenX >= 0 -> inc (m' M.! (y, x - lenX))
            | y - lenY >= 0 -> inc (m' M.! (y - lenY, x))
            | otherwise -> error $ "Bad coords " <> show (x,y)
    inc i = if i == 9 then 1 else i + 1

type Cost = Int
type Path = (Cost, Coord2d)

findMinCostPath :: Map2d Cost -> Maybe Path
findMinCostPath m = bfs (H.empty :: Heap MinPolicy Path) start mark step finish
    where
        start = (0, (0,0))
        mark (cost, coord) = coord
        step (cost, coord) = fmap (first (+cost) . swap) . neighbours4 m $ coord
        end = bottomRight m
        finish (cost, coord) = coord == end

parseInput :: String -> Map2d Int
parseInput = to2dMap . fmap (fmap digitToInt) . lines