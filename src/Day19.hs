module Day19 where

import Puzzle ( Puzzle )
import Util (readAllNums)
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Data.Set (Set)
import Control.Monad (guard)
import Data.Maybe (listToMaybe)

part1 :: Puzzle Int
part1 = S.size . fst . allBeacons . parseInput

part2 :: Puzzle Int
part2 = maxManhattan . snd . allBeacons . parseInput

type Scanner = Set Coord3d
type Coord3d = [Int]

allBeacons :: [Set [Int]] -> (Set [Int], [[Int]])
allBeacons [] = (S.empty, [])
allBeacons (x:xs) = go x xs 0 []
    where
        go x [] _ ds = (x, ds)
        go x (y:ys) i ds = case matchScanners x y of
            (Just (x', d)) -> go x' ys 0 (d:ds)
            Nothing
                | i == 2 * length (y:ys) -> error "Can not find matching beacons"
                | otherwise -> go x (ys ++ [y]) (i + 1) ds

maxManhattan :: (Num a, Ord a) => [[a]] -> a
maxManhattan xs = maximum $ do
    a@[x,y,z] <- xs
    b@[x',y',z'] <- xs
    return $ sum $ zipWith (\x y -> abs (x - y)) a b

matchScanners :: Set [Int] -> Set [Int] -> Maybe (Set [Int], [Int])
matchScanners as bs = listToMaybe $ do
    let aDistances = allDiffs (S.toList as)
    let bDistances = allDiffs (S.toList bs)
    guard $ S.size (aDistances `S.intersection` bDistances) >= sum [1..12]
    a <- drop 11 $ S.toList as
    bs'' <- moveToAllRotations bs
    (bs''',diff) <- moveSetToMatch a bs''
    guard $ commonSizeAtLeast 12 (S.toList bs''')  as
    return (as `S.union` bs''', diff)

commonSizeAtLeast 0 _ bs = True
commonSizeAtLeast _ [] bs = False
commonSizeAtLeast i (x:xs) bs = let i' = if x `S.member` bs then i - 1 else i
                                in i' `seq` commonSizeAtLeast i' xs bs

allDiffs xs = S.fromList $ do
    (i,a) <- zip [1..] xs
    b <- drop i xs
    return $ sum $ zipWith (\x y -> abs (x - y)) a b

moveToAllRotations :: Set [Int] -> [Set [Int]]
moveToAllRotations xs = fmap (\arr -> S.map (rotate arr) xs) rotations
    where
        rotate xyz xs = fmap (\i -> signum i * xs !! (abs i  - 1)) xyz
        (x,y,z) = (1,2,3)
        rotations =
            [ [x,y,z], [x,-z,y], [x,-y,-z], [x,z,-y]
            , [-x,-y,z], [-x,-z,-y], [-x,y,-z], [-x,z,y]

            , [y,-z,-x], [y,x,-z], [y,z,x], [y,-x,z]
            , [-y,z,-x], [-y,x,z], [-y,-z,x], [-y,-x,-z]

            , [z,y,-x], [z,x,y], [z,-y,x], [z,-x,-y]
            , [-z,-y,-x], [-z,x,-y], [-z,y,x], [-z,-x,y]
            ]

-- | Move all elements in a set, so that one of those elements equals `a` 
--   Performs this manipulation for each element of the set
moveSetToMatch:: [Int] -> Set [Int] -> [(Set [Int], [Int])]
moveSetToMatch a bs = do
    b <- drop 11 $ S.toList bs
    let diffs = zipWith (-) a b
    let bs' = S.map (zipWith (+) diffs) bs
    return (bs',diffs)

parseInput :: String -> [Scanner]
parseInput = fmap (S.fromList . fmap readAllNums . tail . lines) . splitOn "\n\n"