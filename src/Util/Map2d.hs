{-# LANGUAGE TupleSections #-}
module Util.Map2d where

import Prelude hiding (lookup)
import Data.Map (Map, fromList, (!), lookup)
import qualified Data.Set as S
import Data.Maybe (mapMaybe)


type Coord2d = (Int, Int)
type Cell2d a = (Coord2d, a)
type Map2d a = Map Coord2d a

-- | Turn a [[a]] into a map of form (y,x) -> a
to2dMap :: [[a]] -> Map2d a
to2dMap ass = fromList [((y,x), a) | (y,as) <- zip [0..] ass, (x,a) <- zip [0..] as]

-- | Get four (up, down, left, right) neighbours of a coordinate
neighbourCoords4 :: Coord2d -> [Coord2d]
neighbourCoords4 (x,y) = [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]

-- | Get four neighbours (coord and values) from a coord and a given 2dMap
-- | non-existing neighbours are ignored
neighbours4 :: Map2d a -> Coord2d -> [Cell2d a]
neighbours4 m c = mapMaybe (\k -> (k,) <$> (k `lookup` m)) (neighbourCoords4 c)

-- | Get all neighbours of a given cell that satisfy a predicate
--   (with respect to their neighbour cell coming from)
--
-- args:
-- - predicate given the former cell and the current cell
-- - the map to work on
-- - the start cell
neighboursSatisfying4 :: Ord a => (Cell2d a -> Cell2d a -> Bool) -> Map2d a -> Coord2d -> S.Set (Cell2d a)
neighboursSatisfying4 p m c = S.unions (s : rest)
    where
        s = S.singleton (c, m ! c)
        rest = go s (c, m ! c) <$> neighbours4 m c
        go cs c c'
            | p c c' = let s = S.singleton c'
                           rest = go (s `S.union` cs) c' <$> neighbours4 m (fst c')
                       in S.unions (s : rest)
            | otherwise = cs