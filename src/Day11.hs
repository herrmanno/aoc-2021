{-# LANGUAGE TupleSections #-}
module Day11 where

import Puzzle ( Puzzle )
import qualified Data.Map as M
import Util.Map2d (Map2d, to2dMap, neighbourCoords8, from2dMap, findMaxValue)
import Util (mmap)
import Data.Char (digitToInt)
import Control.Arrow (Arrow(first))
import Data.Foldable (find)
import Data.Maybe (fromJust)

part1 :: Puzzle
part1 = show . snd . (!!100) . iterate increaseEnergy . parseInput

part2 :: Puzzle
part2 = show . fst . findAllFlashing . zip [0..] . iterate increaseEnergy . parseInput
    where findAllFlashing = fromJust . find ((==0) . sum . fst . snd)

increaseEnergy :: (Map2d Int, Int) -> (Map2d Int, Int)
increaseEnergy (os, n) = finish (flash (os',n))
    where
        os' = M.map (Right . succ) os           -- Right means unflashed, Left means octopus already flashed
        flash (m,n) = case findMaxValue m of -- forall x y. Left x < Right y
            (c, Right i) | i > 9 ->
                let ns = neighbourCoords8 c
                    m' = M.insert c (Left 0) m
                    m'' = foldr (M.adjust (fmap succ)) m' ns
                in flash (m'', n + 1)
            _ -> (m,n)
        finish = let fromEither (Right i) = i
                     fromEither (Left _) = 0
                 in first (M.map fromEither)

parseInput :: String -> (Map2d Int, Int)
parseInput = (,0) . to2dMap . mmap digitToInt . lines