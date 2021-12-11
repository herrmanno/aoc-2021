module Day4 where

import Puzzle ( Puzzle )
import Data.List.Split (splitOn, chunksOf)
import Data.List (transpose, minimumBy, maximumBy)
import Data.Either (isRight, lefts)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Util (mmap)
import qualified Data.Set as S

part1 :: Puzzle Int
part1 s = snd winningScore
    where
        (nums, boards) = parseInput s
        scores = mapMaybe (winAfter nums) boards
        winningScore = minimumBy (comparing fst) scores

part2 :: Puzzle Int
part2 s = snd losingScore
    where
        (nums, boards) = parseInput s
        scores = mapMaybe (winAfter nums) boards
        losingScore = maximumBy (comparing fst) scores


type Board = [[Int]]
type Round = Int
type Score = Int
type BingoNum = Int

-- Returns the round a bingo board wins and it's final score
-- or 'Nothing', if the board never wins
winAfter :: [BingoNum] -> Board -> Maybe (Round, Score)
winAfter xs b = go b S.empty xs
    where
        go b _ [] = Nothing
        go b nums (x:xs)
            | won = Just (S.size nums, x * sum (filter (`S.notMember` nums') (concat b)))
            |otherwise = go b nums' xs
            where
                nums' = S.insert x nums
                won  = any (all (`S.member` nums')) b || any (all (`S.member` nums')) (transpose b)

parseInput :: String -> ([BingoNum], [Board])
parseInput s = (nums, boards)
    where
        (x:xs) = lines s
        nums = map read $ splitOn "," x
        boards = map parseBoard $ chunksOf 6 xs
        parseBoard (x:xs) = map (map read . words) xs
        parseBoard _ = error "bad input: cannot parse board"