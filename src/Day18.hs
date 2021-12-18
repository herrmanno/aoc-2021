module Day18 where

import Puzzle ( Puzzle )
import Util (allPairsWithSwapped)

part1 :: Puzzle Int
part1 = magnitude . foldl1 sumN . fmap parseN . lines

part2 :: Puzzle Int
part2 = maximum . fmap (magnitude . uncurry sumN) . allPairsWithSwapped . fmap parseN . lines

data N = N Int | P N N deriving Eq

instance Show N where
    show (N i) = show i
    show (P l r) = "[" <> show l <> "," <> show r <> "]"

magnitude :: N -> Int
magnitude (N n) = n
magnitude (P l r) = 3 * magnitude l + 2 * magnitude r

sumN :: N -> N -> N
sumN l r = reduce $ P l r

reduce :: N -> N
reduce n = case reduceOne n of
    (n',True) -> reduce n'
    (n',False) -> n'

reduceOne :: N -> (N, Bool)
reduceOne n = case explode n 4 of
    (n', Just _) -> (n', True)
    _ -> split n
    where
        explode (N n) _ = (N n, Nothing)
        explode (P (N l) (N r)) 0 = (N 0, Just (l, r))
        explode p@(P _ _) 0 = error $ "4-level tuple with other tuples inside: " <> show p
        explode (P l r) i =
            let l' = explode l (i - 1)
                r' = explode r (i - 1)
            in case (l',r') of
                ((lval, Just (a,b)), _) -> (P lval (addLeft b r), Just (a,0))
                (_, (rval, Just (a,b))) -> (P (addRight a l) rval, Just (0,b))
                _ -> (P l r, Nothing)
        split (N n)
            | n >= 10 = (P (N (n `div` 2)) (N ((n + 1) `div` 2)), True)
            | otherwise = (N n, False)
        split (P l r) = case (split l, split r) of
            ((l', True), _) -> (P l' r, True)
            (_, (r', True)) -> (P l r', True)
            _ -> (P l r, False)

addLeft :: Int -> N -> N
addLeft i (N n) = N (i + n)
addLeft i (P l r) = P (addLeft i l) r

addRight :: Int -> N -> N
addRight i (N n) = N (i + n)
addRight i (P l r) = P l (addRight i r)

parseN :: String -> N
parseN = fst . parsePair
    where
        parsePair ('[':xs) =
            let (l,',':xs') = parseLeft xs
                (r,']':xs'') = parseRight xs'
            in (P l r, xs'')
        parsePair xs = error $ "Bad input: " <> show xs
        parseLeft xs = case reads xs of
            [(n,rest)] -> (N n, rest)
            _ -> parsePair xs
        parseRight xs = case reads xs of
            [(n,rest)] -> (N n, rest)
            _ -> parsePair xs