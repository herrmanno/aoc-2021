module Day2 where

import Puzzle ( Puzzle )
import Data.Char (toUpper, toLower)

data Dir = Forward Integer | Down Integer | Up Integer deriving Read

capitalise [] = []
capitalise (x:xs) = toUpper x : map toLower xs

solve :: Show a => (b -> Dir -> b) -> b -> (b -> a) -> String -> String
solve f acc finish = show . finish . foldl f acc . fmap (read . capitalise) . lines

part1 :: Puzzle
part1 = solve f (0,0) (uncurry (*))
    where
        f (h,d) (Forward n) = (h + n, d)
        f (h,d) (Down n) = (h, d + n)
        f (h,d) (Up n) = (h, d - n)

part2 :: Puzzle
part2 = solve f (0,0,0) sumUp
    where
        sumUp (h,d,a) = h * d
        f (h,d,a) (Forward n) = (h + n, d + (a * n), a)
        f (h,d,a) (Down n) = (h, d, a + n)
        f (h,d,a) (Up n) = (h, d, a - n)