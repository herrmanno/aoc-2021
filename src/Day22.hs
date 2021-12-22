module Day22 where

import Puzzle ( Puzzle )
import Util (readAllNums)
import Data.Foldable (foldl')

part1 :: Puzzle Int
part1 = sum . map cubeSum . foldCubes . filter validCommand . parseInput
    where validCommand (_, ((a,b),(c,d),(e,f))) = all (\x -> x >= -50 && x <= 50) [a,b,c,d,e,f]

part2 :: Puzzle Int
part2 = sum . map cubeSum . foldCubes . parseInput

type Command = (Bool, Cube)

type Cube = ((Int,Int), (Int,Int), (Int,Int))

cubeSum :: Cube -> Int
cubeSum ((x1,x2),(y1,y2),(z1,z2)) = (x2 + 1 - x1) * (y2 + 1 - y1) * (z2 + 1 - z1)

(x,y,z) `cubeIncludes` (x',y',z') = and $ zipWith tupleIncludes [x,y,z] [x',y',z']

(a,b) `tupleIncludes` (c,d) = c >= a && d <= b

cubesOverlap (x,y,z) (x',y',z') = and $ zipWith tupleOverlap [x,y,z] [x',y',z']

tupleOverlap (a,b) (c,d) =
    (a <= c && c <= b || a <= d && d <= b) || (c <= a && d >= b) || (a <= c && b >= d)

foldCubes :: [Command] -> [Cube]
foldCubes [] = []
foldCubes ((False,_):_) = error "Can not start of 'offcube'"
foldCubes (x:xs) = foldl' go [snd x] xs
    where
        go cubes (True,cube) | any (`cubeIncludes` cube) cubes = cubes
        go cubes (True,cube) =
            let cubes' = filter (not . (cube `cubeIncludes`)) cubes
            in cube : ((`splitCube` cube) =<< cubes')
        go cubes (False,cube) =
            let cubes' = filter (not . (cube `cubeIncludes`)) cubes
            in (`splitCube` cube) =<< cubes'

-- | splits cube `a` into multiple pieces if it overlaps w/ `b`, exluding those parts
--  that that are also in `b`
splitCube :: Cube -> Cube -> [Cube]
splitCube a b | not (cubesOverlap a b) = [a]
splitCube a b = splitCube' a (shrinkCube a b) where
    splitCube' toSplit@(x@(x1,x2),y@(y1,y2),z@(z1,z2)) splitting@(x'@(x1',x2'),y'@(y1',y2'),z'@(z1',z2')) =
        case (x1 < x1', x2 > x2', y1 < y1', y2 > y2', z1 < z1', z2 > z2') of
            -- there is a left part
            (True,_,_,_,_,_) -> ((x1, x1' - 1),y,z) : splitCube ((x1',x2),y,z) splitting
            -- there is a right part
            (_,True,_,_,_,_) -> ((x2' + 1, x2),y,z) : splitCube ((x1,x2'),y,z) splitting
            -- there is a front part
            (_,_,True,_,_,_) -> (x, (y1, y1' - 1),z) : splitCube (x,(y1',y2),z) splitting
            -- there is a back part
            (_,_,_,True,_,_) -> (x, (y2' + 1, y2),z) : splitCube (x,(y1,y2'),z) splitting
            -- there it a bottom part
            (_,_,_,_,True,_) -> (x,y,(z1,z1' - 1)) : splitCube (x,y,(z1',z2)) splitting
            -- there is a top part
            (_,_,_,_,_,True) -> (x,y,(z2' + 1,z2)) : splitCube (x,y,(z1,z2')) splitting
            _ -> []
    shrinkCube to@(x,y,z) from@(x',y',z') = (shrinkTuple x x', shrinkTuple y y', shrinkTuple z z')
    shrinkTuple to@(x,y) from@(x',y') = (max x x', min y y')

parseInput :: String -> [Command]
parseInput = fmap parseCommand . lines
    where parseCommand s = ("on" == head (words s), toCube . readAllNums $ s)
          toCube [x1,x2,y1,y2,z1,z2] = ((x1,x2),(y1,y2),(z1,z2))
