module Util where
import Data.Char (digitToInt)
import Data.List (foldl1')
import qualified Data.Map as M


readBinary' :: [Char] -> Int
readBinary' = binaryToDecimal' . fmap digitToInt

binaryToDecimal' :: [Int] -> Int
binaryToDecimal' = foldl1' (\n bit -> n * 2 + bit)

mmap :: (a -> b) -> [[a]] -> [[b]]
mmap f = map (map f)

upsert :: Ord k => a -> (a -> a) -> k -> M.Map k a -> M.Map k a
upsert val f = M.alter (Just . maybe val f)

range :: (Ord a, Enum a) => a -> a -> [a]
range a b
    | a <= b    = [a..b]
    | otherwise = [b..a]