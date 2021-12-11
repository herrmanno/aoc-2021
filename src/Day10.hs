module Day10 where

import Puzzle ( Puzzle )
import Data.Maybe (mapMaybe)
import Data.List (sort)

part1 :: Puzzle Int
part1 = sum . fmap (sumCorrupts . parseCommandLine) . lines
    where
        sumCorrupts Ok = 0
        sumCorrupts (Incomplete _) = 0
        sumCorrupts (Corrupt ')') = 3
        sumCorrupts (Corrupt ']') = 57
        sumCorrupts (Corrupt '}') = 1197
        sumCorrupts (Corrupt '>') = 25137
        sumCorrupts (Corrupt c) = error $ "Bad char in Corrupt :" <> show c

part2 :: Puzzle Int
part2 = middle . sort . mapMaybe (sumIncomplete . parseCommandLine) . lines
    where
        middle xs = let i = (length xs - 1) `div` 2 in xs !! i
        sumIncomplete Ok = Nothing
        sumIncomplete (Corrupt _) = Nothing
        sumIncomplete (Incomplete xs) =
            let f acc '(' = 5 * acc + 1
                f acc '[' = 5 * acc + 2
                f acc '{' = 5 * acc + 3
                f acc '<' = 5 * acc + 4
                f acc c =  error $ "invalid char: " <> show c
            in Just $ foldl f 0 xs

data ParseResult = Ok | Corrupt Char | Incomplete String deriving Show

parseCommandLine :: String -> ParseResult
parseCommandLine s = go s [] where
    go [] [] = Ok
    go [] ys = Incomplete ys
    go (x:xs) ys
        | x `elem` "([{<"                      = go xs (x:ys)
        | not (null ys) && head ys `matches` x = go xs (tail ys)
        | otherwise                            = Corrupt x
    matches '(' ')' = True
    matches '[' ']' = True
    matches '{' '}' = True
    matches '<' '>' = True
    matches _ _     = False
