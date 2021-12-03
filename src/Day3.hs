module Day3 where

import Puzzle ( Puzzle )
import qualified Data.Map as M
import qualified Data.Bits as B
import Data.Char (digitToInt)
import Util (readBinary', binaryToDecimal')
import Control.Applicative ((<|>))

part1 :: Puzzle
part1 s = show ((*) <$> gammaRate <*> epsilonRate)
    where
        nums =fmap digitToInt <$> lines s
        numLen = length (head (lines s))
        gammaRate = binaryToDecimal' <$> mapM (bitsInPosition nums) [0..numLen - 1]
        epsilonRate = B.xor <$> gammaRate <*> pure (2^numLen - 1)

part2 :: Puzzle
part2 s = show (oRating * co2Rating)
    where
        nums = fmap digitToInt <$> lines s
        oRating = filterByBitRating id nums 1
        co2Rating = filterByBitRating (B.xor 1) nums 0


-- returns the most common bit at given position or Nothing, if both bit values are equally common
bitsInPosition :: [[Int]]   -- ^ the input list as list of list of bits
               -> Int       -- ^ the position; 0 means most significant bit
               -> Maybe Int -- ^ the most common bit at position or 'Nothing'
bitsInPosition xs n = finish $ foldr f (0,0) (fmap (toEnum . (!!n)) xs)
    where
        finish (a,b)
            | a > b = Just 0
            | a < b = Just 1
            | otherwise = Nothing
        f True (a,b) = (a,b+1)
        f False (a,b) = (a+1,b)

-- filter a list of [Int] down by most/least common used bits in every position
filterByBitRating :: (Int -> Int)   -- ^ function to manipualte the most common bit a a given position
                  -> [[Int]]        -- ^ the input numbers
                  -> Int            -- ^ default bit if both bit values are equally common at a position
                  -> Int            -- ^ the one remaining value (as decimal int)
filterByBitRating f xs defaultBit = binaryToDecimal' $ go xs 0
    where
        go [x] _ = x
        go xs n = let Just bit = f <$> bitsInPosition xs n <|> Just defaultBit
                      rest = filter (\x -> x !! n == bit) xs
                  in go rest (n + 1)