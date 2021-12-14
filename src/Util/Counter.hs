module Util.Counter where

import qualified Data.Map as M
import Data.Foldable (maximumBy, minimumBy)
import Data.Ord (comparing)

newtype Counter a = Counter (M.Map a Integer) deriving (Eq, Show)

instance (Ord a) => Semigroup (Counter a) where
    (Counter a) <> (Counter b) = Counter (M.unionWith (+) a b)
instance (Ord a) => Monoid (Counter a) where
    mempty = Counter M.empty
    mappend = (<>)

counter :: Ord a => [a] -> Counter a
counter = Counter . M.fromListWith (+) . (`zip` repeat 1)

fromCounts :: Ord a => [(a, Integer)] -> Counter a
fromCounts = Counter . M.fromList

unCount :: Counter a -> M.Map a Integer
unCount (Counter m) = m

counts :: Counter a -> [(a, Integer)]
counts = M.toList . unCount

maxElement :: Counter a -> (a,Integer)
maxElement (Counter m) = maximumBy (comparing snd) (M.toList m)

minElement :: Counter a -> (a,Integer)
minElement (Counter m) = minimumBy (comparing snd) (M.toList m)

mapKeys :: Ord b => (a -> b) -> Counter a -> Counter b
mapKeys f c = foldMap (\(a,i) -> fromCounts [(f a, i)]) (counts c)