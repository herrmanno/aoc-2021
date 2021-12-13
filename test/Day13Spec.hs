module Day13Spec (spec) where

import Test.Hspec
import Day13 (part1, part2)
import Util.Map2d (PrettyMap(PrettyMap))

spec :: SpecWith ()
spec = do
    describe "day 13" $ do
        it "should solve part 1" $ do
            input <- readFile "input/13.txt"
            part1 input `shouldBe` 942

        it "should solve part 2" $ do
            input <- readFile "input/13.txt"
            let output = PrettyMap
                    [ "..##.####..##..#..#..##..###..###..###."
                    , "...#....#.#..#.#..#.#..#.#..#.#..#.#..#"
                    , "...#...#..#....#..#.#..#.#..#.#..#.###."
                    , "...#..#...#.##.#..#.####.###..###..#..#"
                    , "#..#.#....#..#.#..#.#..#.#....#.#..#..#"
                    , ".##..####..###..##..#..#.#....#..#.###."
                    ]
            part2 input `shouldBe` output