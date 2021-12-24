module Day23 where

import Puzzle ( Puzzle )
import Util.Map2d ( distance)
import Util (range)
import Util.BFS (bfs)
import qualified Data.Heap as H
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Control.Arrow (first)

part1 :: Puzzle (Maybe Cost)
part1 = fmap fst . solve1 2 . parseInput

part2 :: Puzzle (Maybe Cost)
part2 = fmap fst . solve1 4 . prepareInput . parseInput
    where
        prepareInput xs = S.map moveBottom xs `S.union` middleBurrow
        moveBottom (Cave a c 2) = Cave a c 4
        moveBottom (Finished a c 2) = Finished a c 4
        moveBottom a = a
        middleBurrow =
            S.fromList
                    [ Cave D CA 2
                    , Cave D CA 3
                    , Cave C CB 2
                    , Cave B CB 3
                    , Cave B CC 2
                    , Cave A CC 3
                    , Cave A CD 2
                    , Cave C CD 3
                    ]

solve1 :: Int -> Burrow -> Maybe (Cost, Burrow)
solve1 depth burrow = bfs (H.empty :: H.Heap H.MinPolicy (Cost,Burrow)) start mark step finish
    where
        start = (0,burrow)
        mark (c,b) = (c,b)
        step (c,b) = do
            aPos <- S.toList b
            first (+c) <$> stepAmphipod depth aPos b
        finish (_,b) = all isFinished b

type Cost = Int
-- | Amphipod type
data A = A | B | C | D deriving (Eq, Ord, Enum, Show, Read)
-- | Amphipod Room type
data Cave = CA | CB | CC | CD deriving (Eq, Ord, Enum, Show)
-- | Amphipod Position type
data Pos = Hallway A Int | Cave A Cave Int | Finished A Cave Int deriving (Eq, Ord, Show)
type Burrow = S.Set Pos

amphiCost A = 1
amphiCost B = 10
amphiCost C = 100
amphiCost D = 1000

caveX CA = 3
caveX CB = 5
caveX CC = 7
caveX CD = 9

amphiCave = toEnum . fromEnum

isFinished Finished {} = True
isFinished _ = False

allInCave :: Cave -> Burrow -> [(A,Int)]
allInCave cave = mapMaybe f . S.toList where
    f (Cave amph cave' slot) | cave == cave' = Just (amph, slot)
    f (Finished amph cave' slot) | cave == cave' = Just (amph, slot)
    f _ = Nothing

allInHallway :: Burrow -> [(A,Int)]
allInHallway = mapMaybe f . S.toList where
    f (Hallway amph slot) = Just (amph, slot)
    f _ = Nothing

stepAmphipod :: Int -> Pos -> Burrow -> [(Cost,Burrow)]
stepAmphipod _ (Cave a cave pos) b
    | pos /= minimum (snd <$> allInCave cave b) = []
stepAmphipod _ aPos@(Cave a cave pos) b =
    let b' = S.delete aPos b
        currentY = pos
        currentX = caveX cave
        onHallway = snd <$> allInHallway b
        newPos = [ (cost, Hallway a x)
                 | x <- [1,2,4,6,8,10,11]
                 , all (`notElem` onHallway) [ x' | x' <- range currentX x ]
                 , let cost = amphiCost a * distance (currentY, currentX) (0, x)
                 ]
    in fmap (\(c,a') -> (c, S.insert a' b')) newPos
stepAmphipod _ (Hallway a pos) b
    | let inCave = allInCave (amphiCave a) b
      in not (null inCave) && any ((/=a) . fst) inCave = []
stepAmphipod depth aPos@(Hallway a pos) b =
    let b' = S.delete aPos b
        currentX = pos
        currentY = 0
        targetY =
            let inCave = snd <$> allInCave targetCave b
            in if null inCave then depth else minimum inCave - 1
        targetX = caveX targetCave
        targetCave = amphiCave a
        onHallway = snd <$> allInHallway b
        newPos = [ (cost, Finished a targetCave targetY)
                 | let cost = amphiCost a * distance (currentY, currentX) (targetY, targetX)
                 , all (`notElem` onHallway) [ x' | x' <- range currentX targetX, x' /= currentX ]
                 ]
    in fmap (\(c,a') -> (c, S.insert a' b')) newPos
stepAmphipod _ Finished {} _ = []

parseInput :: String -> Burrow
parseInput s = S.fromList $ fmap (f (lines s)) [(y,cave) | y <- [2,3], cave <- [CA,CB,CC,CD]]
    where
        f ls (y,cave) =
            let x = caveX cave
                amphi = read [ls !! y !! x]
            in if amphiCave amphi == cave && y == 3
                then Finished amphi cave (y - 1)
                else Cave amphi cave (y - 1)