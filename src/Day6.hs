module Day6 where

import Puzzle ( Puzzle )
import Data.List.Split (splitOn)

part1 :: Puzzle Integer
part1 = solve 80

part2 :: Puzzle Integer
part2 = solve 256

solve d = sum . map ((+1) . childrenAfterDay d) . parseInput
    where
        childrenAfterDay d f = let birthDay = d + 8 - f in childrenAfterDaysToLive birthDay

{-| calculates the summed up number of children a fish gives birth to, if he lives `d` days
    *after his birth*
-}
childrenAfterDaysToLive = (memo !!)
    where
        -- fish don't give birth in their first 8 days
        memo = replicate 8 0 ++ map f [8..]
        -- fish give first birth on their 9th day, then every other 7th day
        f i = sum . map ((+1) . (memo !!)) . takeWhile (>=0) $ iterate (subtract 7) (i - 9)
        --                ^      ^ the number of grandchildren
        --                | add 1 to the number of grandchildren for the child fish itself

parseInput = map read . splitOn ","