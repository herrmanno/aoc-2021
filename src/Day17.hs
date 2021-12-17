module Day17 where

import Puzzle ( Puzzle )
import Data.Bifunctor (bimap)
import Data.Maybe (catMaybes)
import Util (readAllNums)

part1 :: Puzzle Int
part1 = maximum . fmap snd . concat . allShootsIntoTarget . parseInput

part2 :: Puzzle Int
part2 = length . allShootsIntoTarget . parseInput

type TargetArea = ((Int,Int), (Int,Int))

inArea :: TargetArea -> (Int,Int) -> Bool
inArea ((xMin,xMax), (yMin,yMax)) (x,y) = x <= xMax && x >= xMin && y <= yMax && y >= yMin

missesArea :: TargetArea -> (Int,Int) -> Bool
missesArea ((xMin,xMax), (yMin,yMax)) (x,y) = x > xMax || y < yMin

shoot :: TargetArea -> (Int, Int) -> Maybe [(Int, Int)]
shoot area = go (0,0)
    where
        go p v@(vx,vy)
            | inArea area p = Just [p]
            | missesArea area p = Nothing
            | otherwise =
                let p' = bimap (+vx) (+vy) p
                    v' = bimap dec pred v
                in (:) <$> Just p <*> go p' v'
        dec = max 0 . pred

allShootsIntoTarget :: TargetArea -> [[(Int, Int)]]
allShootsIntoTarget area@((minX,maxX), (minY,maxY)) = catMaybes paths
    where
        paths = [shoot area (x,y) | x <- xVelocities, y <- yVelocities]
        {-
            x-velocities where sum [1..xVel] < minX will never reach minX because they will decrease
            to zero before reaching the target area.
            x-velocities > maxX will overshoot the target in the first step already.
        -}
        xVelocities =
            let (minX':_) = dropWhile ((<minX) . sum . enumFromTo 1) [1..]
            in [minX'..maxX]
        {-
            y-velocities lower than minY will always miss the target because they will go to low.
            Apparently, the maximal y-velocity in the test case and my input is `(-1) * minY - 1`
            I don't know why this is so, but it works anyway.
        -}
        yVelocities = [minY..(-1)*minY]

parseInput :: String -> TargetArea
parseInput s = let [minX,maxX,minY,maxY] = readAllNums s in ((minX,maxX), (minY,maxY))