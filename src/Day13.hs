module Day13 where

import Puzzle ( Puzzle )
import Util.Map2d (Map2d, pretty, PrettyMap)
import Data.List.Split (splitOn)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Tuple (swap)

part1 :: Puzzle Int
part1 s = S.size $ applyFold coords (head folds)
    where (coords, folds) = parseInput s

part2 :: Puzzle PrettyMap
part2 s = pretty prettyPrint (foldr (uncurry M.insert) M.empty (zip finalCoords (repeat True)))
    where
        (coords, folds) = parseInput s
        finalCoords = swap <$> S.toList (foldl applyFold coords folds)
        prettyPrint (Just _) = '#'
        prettyPrint Nothing = '.'


applyFold :: S.Set (Int, Int) -> Either Int Int -> S.Set (Int, Int)
applyFold xs (Left n)  = S.map f xs where f (x,y) = (n - abs (x - n), y)
applyFold xs (Right n) = S.map f xs where f (x,y) = (x, n - abs (y - n))

parseInput :: String -> (S.Set (Int,Int), [Either Int Int])
parseInput s = (coords, folds)
    where
        [coordss, foldss] = splitOn [""] (lines s)
        coords = S.fromList $ fmap (\s -> read ("(" <> s <> ")")) coordss
        folds = let f [_,_,x:_:xs]
                        | x == 'x' = Left (read xs)
                        | otherwise = Right (read xs)
                    f xs = error $ "Bad fold input: " <> unwords xs
                in fmap (f . words) foldss