module Day9 where

import Puzzle ( Puzzle )
import Util (to2dMap, neighbourCoords4, mmap, neighbours4, Map2D, Coord)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (digitToInt)
import Data.List (sortOn)
import Data.Ord (Down(Down))

part1 :: Puzzle
part1 = show . solve1 . parseInput

part2 :: Puzzle
part2 = show . solve2 . parseInput

solve1 :: M.Map Coord Int -> Int
solve1 m = (\xs -> sum xs + length xs) . fmap (m M.!) . lowPoints $ m

solve2 :: M.Map Coord Int -> Int
solve2 m = (product . take 3 . sortOn Down) (S.size . toBasin m <$> lowPoints m)

lowPoints :: M.Map Coord Int -> [Coord]
lowPoints m = M.keys . M.filterWithKey filterLowPoint $ m
    where filterLowPoint coord value = all (>value) (snd <$> neighbours4 m coord)

toBasin :: M.Map Coord Int -> Coord -> S.Set Coord
toBasin m coord = go coord (S.singleton coord)
    where go c cs =
            let a = m M.! c
                -- neighbour coords that are higher (but not to high) than the curren coord
                -- and are not yet processed
                ns = filter (`S.notMember` cs)
                   . map fst
                   . filter (\(_,b) -> b > a && b < 9)
                   . neighbours4 m
                   $ c
                -- new set of processed node (that belong to the basin)
                cs' = cs `S.union` S.singleton c `S.union` S.fromList ns
            in if null ns then cs' else S.unions (fmap (`go` cs') ns)

parseInput = to2dMap . mmap digitToInt . lines
