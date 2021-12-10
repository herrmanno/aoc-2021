module Day8 where

import Puzzle ( Puzzle )
import Data.List.Split (splitOn)
import Util (toTuple, count)
import qualified Data.Map as M

part1 :: Puzzle
part1 = show . count (`elem` ['1','4','7','8']) . concatMap (show . solve) . parseInput

part2 :: Puzzle
part2 = show . sum . map solve . parseInput

type Input = ([String], [String]) -- ^ list of words left/rught of '|' char in input line

solve :: Input -> Int
solve (xs,ys) = foldl1 (\acc a -> 10 * acc + a) nums
    where
        nums = map (lookupNumber . sum . map (counts M.!)) ys
        -- wire usages over all 10 digits: a: 8, b: 6, c: 8, d: 7, e: 4, f: 9, g: 7
        counts = M.fromListWith (+) (zip (concat xs) (repeat 1))
        lookupNumber 17 = 1 -- c(8) + f(9)
        lookupNumber 30 = 4 -- b(6) + c(8) + d(7) + f(9)
        lookupNumber 25 = 7 -- a(8) + c(8) + f(9)
        lookupNumber 49 = 8 -- a(8) + b(6) + c(8) + d(7) + e(4) + f(9) + g(7)
        lookupNumber 42 = 0 -- a(8) + b(6) + c(8) + e(4) + f(9) + g(7)
        lookupNumber 41 = 6 -- a(8) + b(6) + d(7) + e(4) + f(9) + g(7)
        lookupNumber 45 = 9 -- a(8) + b(6) + c(8) + d(7) + f(9) + g(7)
        lookupNumber 34 = 2 -- a(8) + c(8) + d(7) + e(4) + g(7)
        lookupNumber 39 = 3 -- a(8) + c(8) + d(7) + f(9) + g(7)
        lookupNumber 37 = 5 -- a(8) + b(6) + d(7) + f(9) + g(7)
        lookupNumber n = error $ "Bad digit sum: " <> show n

parseInput :: String -> [Input]
parseInput = fmap (toTuple . fmap words . splitOn "|") . lines