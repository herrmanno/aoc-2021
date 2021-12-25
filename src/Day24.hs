{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day24 where

import Puzzle ( Puzzle )
import qualified Data.Map as M
import Data.Foldable (Foldable(foldl'))
import Debug.Trace
import Data.List (tails)
import Control.Monad (guard)
import Data.Maybe (listToMaybe)
import Data.Char (intToDigit)
import Data.List.Split (chunksOf)

part1 :: Puzzle Integer
part1 = read . fmap intToDigit . fst . solve head . parseParams

part2 :: Puzzle Integer
part2 = read . fmap intToDigit . fst . solve last . parseParams

findPair :: Int -> Int -> [(Int,Int)]
findPair a b = do
    x <- [9,8..1]
    y <- [9,8..1]
    guard $ x + a + b - y == 0
    return (x,y)

solve :: ([(Int,Int)] -> (Int,Int)) -> [Param] -> ([Int],[Param])
solve _ [] = ([],[])
solve f ((1,_,p):(26,p',_):ps) =
    let (a,b) = f $ findPair p p'
    in ([a,b],ps)
solve f (p:ps) =
    let (middle,ps') = solve f ps
        (front:back,ps'') = solve f (p:ps')
    in (front : middle ++ back, ps'')


type Param = (Int,Int,Int)

parseParams :: String -> [Param]
parseParams = fmap toParams . chunksOf 18 . lines
    where
        toParams xs =
            let a = read $ last $ words (xs !! 4)
                b = read $ last $ words (xs !! 5)
                c = read $ last $ words (xs !! 15)
            in (a,b,c)