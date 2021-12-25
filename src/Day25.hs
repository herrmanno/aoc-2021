module Day25 where

import Puzzle ( Puzzle )
import Util.Map2d (Coord2d, to2dMap, size)
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Arrow (second, first)

part1 :: Puzzle Int
part1 = stepUntil . parseMaze

part2 :: Puzzle Int
part2 = undefined

type Easts = S.Set Coord2d
type Souths = S.Set Coord2d
type Size = Coord2d
type Maze = (Easts,Souths,Size)

step :: Maze -> Maze
step (es,ss,(y,x)) = (es',ss',(y,x))
    where
        es' = S.map moveEast es
        ss' = S.map moveSouth ss
        moveEast coord =
            let coord' = second ((`mod` x) . succ) coord
            in if coord' `S.member` es || coord' `S.member` ss
                then coord
                else coord'
        moveSouth coord =
            let coord' = first ((`mod` y) . succ) coord
            in if coord' `S.member` es' || coord' `S.member` ss
                then coord
                else coord'

stepUntil :: Maze -> Int
stepUntil m = go 0 m where
    go n m = let m' = step m
                 n' = n + 1
             in if m == m' then n' else go n' m'

parseMaze :: String -> Maze
parseMaze s = (easts,souths,borders)
    where
        m = to2dMap (lines s)
        easts = S.fromList . fmap fst . filter ((=='>') .snd) $ M.toList m
        souths = S.fromList . fmap fst . filter ((=='v') .snd) $ M.toList m
        borders = size m
