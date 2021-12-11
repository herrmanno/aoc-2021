module Day1 where

import Puzzle ( Puzzle )
import Data.List.Split (chunksOf)
import Control.Arrow ((>>>), (&&&))

part1 :: Puzzle Int
part1 = lines
    >>> map (read @Int)
    >>> (zipWith (<) <$> id <*> tail)
    >>> filter id
    >>> length

part2 :: Puzzle Int
part2 = lines
    >>> map (read @Int)
    >>> (toWindows &&& (toWindows . tail))
    >>> uncurry (zipWith (<))
    >>> filter id
    >>> length
    where
        toWindows = map sum . toWindowsOf 3

toWindowsOf :: Int -> [a] -> [[a]]
toWindowsOf n [] = []
toWindowsOf n xss@(_:xs)
    | length xss < n = []
    | otherwise = take n xss : toWindowsOf n xs