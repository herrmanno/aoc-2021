module Day9 where

import Puzzle ( Puzzle )
import Util (mmap)
import Util.Map2d (to2dMap, neighbours4, Map2d, Coord2d, neighboursSatisfying4)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (digitToInt)
import Data.List (sortOn)
import Data.Ord (Down(Down))

part1 :: Puzzle Int
part1 = solve1 . parseInput

part2 :: Puzzle Int
part2 = solve2 . parseInput

solve1 :: Map2d Int -> Int
solve1 m = (\xs -> sum xs + length xs) . fmap (m M.!) . lowPoints $ m

solve2 :: Map2d Int -> Int
solve2 m = (product . take 3 . sortOn Down) (S.size . toBasin m <$> lowPoints m)

lowPoints :: Map2d Int -> [Coord2d]
lowPoints m = M.keys . M.filterWithKey filterLowPoint $ m
    where filterLowPoint coord value = all (>value) (snd <$> neighbours4 m coord)

toBasin :: Map2d Int -> Coord2d -> S.Set Coord2d
toBasin m = S.map fst . neighboursSatisfying4 p m
    where p (_,a) (_,a') = a' > a && a' /= 9

parseInput :: String -> Map2d Int
parseInput = to2dMap . mmap digitToInt . lines
