module Day8 where

import Puzzle ( Puzzle )
import Data.List.Split (splitOn)
import Util (toTuple, count)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Foldable (find)
import Data.List (group, sort)
import Data.Tuple (swap)
import Data.Bifunctor (Bifunctor(bimap))

part1 :: Puzzle
part1 = show . count ((`S.member` uniques) . length) . concatMap snd . parseInput
    where uniques = S.fromList [2,4,3,7]

part2 :: Puzzle
part2 = show . sum. map solve . parseInput

type Input = ([String], [String])
type Display = M.Map Char (S.Set Char)

solve :: Input -> Int
solve (xs,ys) =
    -- first, deduce common pattern from unique* numbers (by their count of display segments used)
    let d1 = let Just c1 = find ((==2) . length) xs in foldr (M.adjust (S.intersection (S.fromList c1))) display ("cf"::String)
        d4 = let Just c4 = find ((==4) . length) xs in foldr (M.adjust (S.intersection (S.fromList c4))) d1 ("bcdf"::String)
        d7 = let Just c7 = find ((==3) . length) xs in foldr (M.adjust (S.intersection (S.fromList c7))) d4 ("acf"::String)
        d8 = let Just c8 = find ((==7) . length) xs in foldr (M.adjust (S.intersection (S.fromList c8))) d7 ("abcdefg"::String)
    -- then run further deductions and reverse the resulting map
        d' = reverseMap $ foldl (\m f -> f m) d8 deductions
        nums = map (getNum d') ys
    in foldl1 (\acc a -> 10 * acc + a) nums
    where
        display = M.fromList (zip "abcdefg" (repeat (S.fromList "abcdefg")))
        deductions =
            [ \m -> M.adjust (S.\\ (m M.! 'c')) 'a' m                           -- 'a' is '7' \\ '1'
            , \m -> foldr (M.adjust (S.\\ (m M.! 'a'))) m ("bcdefg"::String)    -- 'a' can not be part of any other number (except a)
            , \m -> foldr (M.adjust (S.\\ (m M.! 'c'))) m ("abdeg"::String)     -- 'c' can not be part of any other number (except c and f)
            , \m -> foldr (M.adjust (S.\\ (m M.! 'b'))) m ("eg"::String)        -- 'b' can not be part of e or g
            -- to this point there are three tuples left, each sharing to wires: (c,f), (b,d), (e,g)
            , M.adjust (S.intersection count9) 'f'                              -- 'f' occurs in 9 different digits
            , M.adjust (S.intersection count8) 'c'                              -- 'c' occurs in 8 different digits
            , M.adjust (S.intersection count7) 'd'                              -- 'd' occurs in 7 different digits
            , M.adjust (S.intersection count6) 'b'                              -- 'b' occurs in 6 different digits
            , M.adjust (S.intersection count4) 'e'                              -- 'd' occurs in 7 different digits
            , M.adjust (S.intersection count7) 'g'                              -- 'b' occurs in 6 different digits
            ]
        count9 = S.fromList . map head . filter ((==9) . length) . group . sort . concat $ xs
        count8 = S.fromList . map head . filter ((==8) . length) . group . sort . concat $ xs
        count7 = S.fromList . map head . filter ((==7) . length) . group . sort . concat $ xs
        count6 = S.fromList . map head . filter ((==6) . length) . group . sort . concat $ xs
        count4 = S.fromList . map head . filter ((==4) . length) . group . sort . concat $ xs
        reverseMap = M.fromList . map (bimap S.findMin S.singleton . swap) . M.toList
        getNum display n = getNumberFromWires $ S.unions (map (display M.!) n)

getNumberFromWires :: Num p => S.Set Char -> p
getNumberFromWires s
    | s == S.fromList "abcefg"    = 0
    | s == S.fromList "cf"        = 1
    | s == S.fromList "acdeg"     = 2
    | s == S.fromList "acdfg"     = 3
    | s == S.fromList "bcdf"      = 4
    | s == S.fromList "abdfg"     = 5
    | s == S.fromList "abdefg"    = 6
    | s == S.fromList "acf"       = 7
    | s == S.fromList "abcdefg"   = 8
    | s == S.fromList "abcdfg"    = 9
    | otherwise                   = error $ "Bad input wire set: " <> show s


parseInput :: String -> [Input]
parseInput = fmap (toTuple . fmap words . splitOn "|") . lines