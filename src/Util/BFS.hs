module Util.BFS where

import Data.Heap (HeapItem, HeapT, Prio, Val)
import qualified Data.Heap as H
import qualified Data.Set as S

-- | Does a breath-first-search
bfs :: (Ord b, HeapItem pol a)
    => HeapT (Prio pol a) (Val pol a)   -- ^ the initial heap
    -> a                                -- ^ the start element
    -> (a -> b)                         -- ^ select the element part that identifies it as visited
    -> (a -> [a])                       -- ^ selects the next elements
    -> (a -> Bool)                      -- ^ identifies an element as end of search
    -> Maybe a                          -- ^ the final element, if any is found before the heap runs empty
bfs heap start mark step finish = go (H.insert start heap) (S.singleton (mark start))
    where
        go next visited = do
            (a, next') <- H.view next
            let ns = filter ((`S.notMember` visited) . mark) (step a)
            let next'' = foldr H.insert next' ns
            let visited' = foldr S.insert visited (fmap mark ns)
            if finish a then pure a else go next'' visited'