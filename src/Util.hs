module Util where
import Data.Char (digitToInt)
import Data.List (foldl1')


readBinary' :: [Char] -> Int
readBinary' = binaryToDecimal' . fmap digitToInt

binaryToDecimal' :: [Int] -> Int
binaryToDecimal' = foldl1' (\n bit -> n * 2 + bit)

mmap :: (a -> b) -> [[a]] -> [[b]]
mmap f = map (map f)