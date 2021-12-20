module Day19 where

import Puzzle ( Puzzle )
import Util (readAllNums)
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Data.List (sort, group, permutations, elemIndex)
import Control.Monad (guard)
import Data.Maybe (listToMaybe, mapMaybe)
import Debug.Trace

part1 :: Puzzle Int
part1 = S.size . fst . allBeacons . parseInput

part2 :: Puzzle Int
part2 = maxManhattan . snd . allBeacons . parseInput


type Scanner = S.Set Coord3d
type Coord3d = [Int]

allBeacons :: [S.Set [Int]] -> (S.Set [Int], [[Int]])
allBeacons [] = (S.empty, [])
allBeacons (x:xs) = go x xs 0 []
    where
        go x [] _ ds = (x, ds)
        go x (y:ys) i ds = case matchScanners x y of
            (Just (x', d)) -> go x' ys 0 (d:ds)
            Nothing
                | i == length (y:ys) -> error "Can not find matching beacons"
                | otherwise -> go x (ys ++ [y]) (i + 1) ds

maxManhattan :: (Num a, Ord a) => [[a]] -> a
maxManhattan xs = maximum $ do
    a@[x,y,z] <- xs
    b@[x',y',z'] <- xs
    return $ sum $ zipWith (\x y -> abs (x - y)) a b

matchScanners :: S.Set [Int] -> S.Set [Int] -> Maybe (S.Set [Int], [Int])
matchScanners as bs = listToMaybe $ do
    -- let aDistances = allDiffs (S.toList as)
    -- let bDistances = allDiffs (S.toList bs)
    -- guard $ S.size (aDistances `S.intersection` bDistances) >= 80
    a <- S.toList as
    bs' <- moveToAllDirections bs
    bs'' <- moveToAllPermutations bs'
    (bs''',diff) <- moveSetToMatch a bs''
    let common = as `S.intersection` bs'''
    guard $ S.size common >= 12
    return (as `S.union` bs''', diff)

allDiffs xs = S.fromList $ do
    (i,a) <- zip [1..] xs
    b <- drop i xs
    return $ sum $ zipWith (\x y -> abs (x - y)) a b

moveToAllDirections :: S.Set [Int] -> [S.Set [Int]]
moveToAllDirections xs = fmap (\f -> S.map (zipWith ($) f) xs) directions
    where
        directions = [ [a,b,c] | a <- [id,negate], b <- [id,negate], c <- [id,negate] ]
        -- directions = [ [id,id,id], [id,negate,negate]
        --              , [negate,negate,id], [negate,id,negate]
        --              ]

moveToAllPermutations :: Ord c => S.Set [c] -> [S.Set [c]]
moveToAllPermutations xs = fmap (\p -> S.map (rotate p) xs) ps
    where
        ps = permutations [0,1,2]
        rotate p xs = map (xs !!) p

-- | Move all elements in a set, so that one of those elements equals `a` 
--   Performs this manipulation for each element of the set
moveSetToMatch:: (Show b, Eq b, Num b, Ord b) => [b] -> S.Set [b] -> [(S.Set [b], [b])]
moveSetToMatch a bs = do
    b <- S.toList bs
    let diffs = zipWith (-) a b
    let bs' = S.map (zipWith (+) diffs) bs
    return (bs',diffs)

parseInput :: String -> [Scanner]
parseInput = fmap (S.fromList . fmap readAllNums . tail . lines) . splitOn "\n\n"