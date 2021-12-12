module Day12 where

import Puzzle ( Puzzle )
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Util (toTuple, upsert)
import Data.Char (isUpper)
import qualified Data.Set as S
import Control.Arrow ((&&&))

part1 :: String -> Int
part1 = length . findWaysFromStarToEnd (emptyCache @SingleCaveCache) . parseInput

part2 :: String -> Int
part2 = length . findWaysFromStarToEnd (emptyCache @MultiCaveCache) . parseInput

data Cave = Start | End | Big String | Small String deriving (Eq, Ord, Show)

mkCave "start" = Start
mkCave "end" = End
mkCave s
    | all isUpper s = Big s
    | otherwise  = Small s

class CaveCache a where
    -- | Visits a cave and marks it as visited inside the new returned cache
    visit :: Cave -> a -> a
    visit (Big _) cache = cache
    visit End cache = cache
    visit Start cache = illegalCache
    visit cave cache = canVisit cave cache

    -- | Checks if a cache can be visited and returns a new cache that represents this cache as visited
    canVisit :: Cave -> a -> a

    -- | Checks if this cache represents a valid path of visited caves
    isLegal :: a -> Bool

    -- | Returns the default empty cache
    emptyCache :: a

    -- | Returns the default illegal cache
    illegalCache :: a

-- | A cache that forbis multiple visits of small caves
data SingleCaveCache = SingleCaveCache (S.Set Cave) | SingleCaveCacheIllegal

instance CaveCache SingleCaveCache where
    emptyCache = SingleCaveCache S.empty
    illegalCache = SingleCaveCacheIllegal
    canVisit cave cache@(SingleCaveCache set)
        | cave `S.member` set = SingleCaveCacheIllegal
        | otherwise = SingleCaveCache (S.insert cave set)
    canVisit _ cache = cache
    isLegal (SingleCaveCache _) = True
    isLegal _ = False

-- | A cache that allows a single small cave to be visited twice
data MultiCaveCache = MultiCaveCache (S.Set Cave) | MultiCaveCacheLimit Cave (S.Set Cave) | MultiCaveCacheIllegal

instance CaveCache MultiCaveCache where
    emptyCache = MultiCaveCache S.empty
    illegalCache = MultiCaveCacheIllegal
    canVisit cave cache@(MultiCaveCache set)
        | cave `S.member` set = MultiCaveCacheLimit cave set
        | otherwise = MultiCaveCache (S.insert cave set)
    canVisit cave cache@(MultiCaveCacheLimit limit set)
        | cave == limit = MultiCaveCacheIllegal
        | cave `S.member` set = MultiCaveCacheIllegal
        | otherwise = MultiCaveCacheLimit limit (S.insert cave set)
    canVisit _ cache = cache

    isLegal (MultiCaveCache _) = True
    isLegal (MultiCaveCacheLimit _ _) = True
    isLegal _ = False


findWaysFromStarToEnd :: CaveCache c => c -> Map Cave [Cave] -> [[Cave]]
findWaysFromStarToEnd cache m = go Start cache
    where
        go End _ = [[End]]
        go node cache =
            let neighbours = m ! node
                nexts = filter (isLegal . snd) $ fmap (id &&& (`visit` cache)) neighbours
                remainingPaths = fmap (uncurry go) nexts
            in fmap (node :) =<< remainingPaths

parseInput :: String -> Map Cave [Cave]
parseInput = foldr insertCave M.empty . fmap parseLine . lines
    where
        insertCave (a,b) = upsert [a] (a :) b . upsert [b] (b :) a
        parseLine = toTuple . fmap mkCave . splitOn "-"