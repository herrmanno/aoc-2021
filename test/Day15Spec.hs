module Day15Spec (spec) where

import Test.Hspec
import Day15 (part1, part2)

spec :: SpecWith ()
spec = do
    describe "day 15" $ do
        it "should solve part 1" $ do
            input <- readFile "input/15.txt"
            part1 input `shouldBe` 435

        it "should solve part 2" $ do
            input <- readFile "input/15.txt"
            part2 input `shouldBe` 2842