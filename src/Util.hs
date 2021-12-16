module Util where
import Data.Char (digitToInt, ord)
import Data.List (foldl1')
import qualified Data.Map as M
import Data.Bits as B


--------------------
-- misc utilities --
--------------------

-- | convert a [a,a] to (a,a)
--
-- is a **partial function**
toTuple [a,b] = (a,b)
toTuple xs = error $ "cannot turn list w/ '" <> show (length xs) <> "' into tuple"

-- fmap inside a two level deep functor
mmap :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
mmap f = fmap (fmap f)

-- modify a map value if the key exists or insert a default value instead
upsert :: Ord k => a -> (a -> a) -> k -> M.Map k a -> M.Map k a
upsert val f = M.alter (Just . maybe val f)

-- the list from a..b, or b..a, iff b < b
range :: (Ord a, Enum a) => a -> a -> [a]
range a b
    | a <= b    = [a..b]
    | otherwise = [b..a]

-- the sum of 1..n
sumN :: Integral a => a -> a
sumN n = (n * (n + 1)) `div` 2

-- | Return the number of elements that satisfy a predicate
count :: (a -> Bool) -> [a] -> Int
count p = length . filter p


------------------------------
-- binary number conversion --
------------------------------

readHex :: Char -> Int
readHex c
    | ord c >= 48 && ord c <= 57 = ord c - 48
    | ord c >= 65 && ord c <= 70 = ord c - 55
    | otherwise = error $ "Non hex-digit: " <> show c <> "(" <> show (ord c) <> ")"

decimalToBinary :: Int -> Int -> [Int]
decimalToBinary len i = fmap (fromEnum . B.testBit i) [len-1,len-2..0]

readBinary' :: [Char] -> Int
readBinary' = binaryToDecimal' . fmap digitToInt

binaryToDecimal' :: [Int] -> Int
binaryToDecimal' = foldl1' (\n bit -> n * 2 + bit)
