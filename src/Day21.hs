module Day21 where

import Puzzle ( Puzzle )
import qualified Data.Map as M
import Data.Bits (Bits(xor))
import Data.Bifunctor (bimap)
import Control.Arrow ((&&&))

part1 :: Puzzle Int
part1 = uncurry play . parseInput

part2 :: Puzzle Integer
part2 s = max r1 r2 where
    (p1,p2) = parseInput s
    (r1,r2) = play' 21 p1 p2

play :: Int -> Int -> Int
play p1 p2 = go (p1,0) (p2,0) 0 die where
    go a@(pos,a_score) b@(_,b_score) n die
        | a_score >= 1000 = n * b_score
        | b_score >= 1000 = n * a_score
        | otherwise =
            let val = sum (take 3 die)
                die' = drop 3 die
                pos' = move (pos + val)
                a' = (pos', a_score + pos')
            in go b a' (n+3) die'

die = cycle [1..100]


play' :: Int -> Int -> Int -> (Integer,Integer)
play' limit p1 p2 =
    let (p1Wins,_) = memo M.! (p1, 0, p2, 0, True)
        (_,p2Wins) = memo M.! (p1, 0, p2, 0, False)
    in (p1Wins, p2Wins)
    where
        memo = M.fromList $ fmap (id &&& f) allStates
        allStates = [ (p1,s1,p2,s2,moveP1)
                    | p1 <- [1..10]
                    , p2 <- [1..10]
                    , s1 <- [0..limit]
                    , s2 <- [0..limit]
                    , moveP1 <- [False,True]
                    ]
        f (p1,21,p2,s2,_) = (1,0)
        f (p1,s1,p2,21,_) = (0,1)
        f (p1,s1,p2,s2,moveP1) =
            let ss1' = turn (p1,s1)
                ss2' = turn (p2,s2)
                ss' = if moveP1
                    then [ (n, (p1',min limit s1',p2,s2, moveP1 `xor` True)) | (n, (p1',s1')) <- ss1' ]
                    else [ (n, (p1,s1,p2',min limit s2', moveP1 `xor` True)) | (n, (p2',s2')) <- ss2' ]
                results = fmap (\(n,s') -> bimap (*n) (*n) (memo M.! s')) ss'
            in foldTuples results
        -- | returns all possible new (pos,score) pairs together with their count
        turn (p,s) = do
            (val,n') <- [(3,1), (4,3), (5,6), (6,7), (7,6), (8,3), (9,1)]
            let p' = move (p + val)
                s' = s + p'
            return (n', (p',s'))
        foldTuples = foldr (\(a,b) (c,d) -> (a+c, b+d)) (0,0)

        {- same approch but w/o memoization; takes a lot more time to run! -}
        -- go a@(aTag, (pos,a_score)) b@(bTag, (_,b_score)) n
        --     | a_score >= limit = return $! aTag * n
        --     | b_score >= limit = return $! bTag * n
        --     | otherwise = do
        --         (val,n') <- [(3,1), (4,3), (5,6), (6,7), (7,6), (8,3), (9,1)]
        --         let pos' = move (pos + val)
        --             a' = (aTag, (pos', a_score + pos'))
        --             rest = go b a' (n*n')
        --         return $! sum $! rest

move n = case n `rem` 10 of
    0 -> 10
    n' -> n'

parseInput :: String -> (Int, Int)
parseInput s = (a,b) where
    [a,b] = fmap (read . last . words) . lines $ s