module Day14 where

import Puzzle ( Puzzle )
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Util.Counter (counter, counts, fromCounts, maxElement, minElement, mapKeys)
import Day1 (toWindowsOf)
import Data.Foldable (foldMap')

part1 :: Puzzle Integer
part1 = uncurry (flip $ solve 10) . parseInput

part2 :: Puzzle Integer
part2 = uncurry (flip $ solve 40) . parseInput

type Rules = M.Map String Char

solve :: Int -> Rules -> String -> Integer
solve n m s = snd (maxElement characterCounts) - snd (minElement characterCounts)
    where
        characterCounts = fromCounts [(head s, 1)] <> mapKeys (!!1) windowCounts
        windowCounts = let windows = toWindowsOf 2 s in iterate step (counter windows) !! n
        -- | replaces the occurence of ('ab', i) in a counter with [(ac, i), (cb,i)] where
        --  'c' is the result of the rule 'ab -> c', if there is any such rule for 'ab', otherwise
        --  just returns the original counter
        --
        --  This replacement is done for all keys in the counter and is repeated 'n' times
        step c = foldMap' (f c) (counts c) where
            f c (seq@[a,b], i) =
                maybe c (\char -> fromCounts [([a,char], i), ([char,b], i)]) (M.lookup seq m)
            f c (seq, _) = error $ "Bad sequence: " <> show seq

parseInput :: String -> (String, Rules)
parseInput s = (template, rules)
    where
        [ss1, ss2] = splitOn [""] (lines s)
        [template] = ss1
        rules = M.fromList (fmap parseRule ss2)
        parseRule s = let (from:_:to:_) = splitOn " " s in (from, head to)