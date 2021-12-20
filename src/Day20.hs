module Day20 where

import Puzzle ( Puzzle )
import Util.Map2d (to2dMap, Map2d, size)
import qualified Data.Map as M
import Data.Bifunctor (bimap)
import Util (binaryToDecimal', count)

part1 :: Puzzle Int
part1 s = count (==1) . M.elems . snd . (!!2) . iterate (uncurry (transform code)) $ (0, image)
    where (code, image) = parseInput s

part2 :: Puzzle Int
part2 s = count (==1) . M.elems . snd . (!!50) . iterate (uncurry (transform code)) $ (0, image)
    where (code, image) = parseInput s

type Code = [Int]
type Image = Map2d Int

transform :: Code -> Int -> Image -> (Int, Image)
transform cs i m = ( if i == 0 then head cs else last cs
                   , M.fromList [ ((y,x), f y x) | y <- [0..yMax], x <- [0..xMax]]
                   )
    where
        f y x =
            let binValue = [ M.findWithDefault i (y',x') m' | y' <- [y-1,y,y+1], x' <- [x-1,x,x+1]]
                index = binaryToDecimal' binValue
            in cs !! index
        m' = M.mapKeys (bimap succ succ) m
        (yMax,xMax) = size m'

parseInput :: String -> (Code, Map2d Int)
parseInput s = (fmap toDigit cs, to2dMap (fmap (fmap toDigit) rest))
    where (cs:_:rest) = lines s
          toDigit '#' = 1
          toDigit _   = 0