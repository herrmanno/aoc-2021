module Day16Spec (spec) where

import Test.Hspec
import Day16 (part1, part2)

spec :: SpecWith ()
spec = do
    describe "day 16" $ do
        it "should solve part 1" $ do
            input <- readFile "input/16.txt"
            part1 input `shouldBe` 893

        it "should solve part 2" $ do
            input <- readFile "input/16.txt"
            part2 input `shouldBe` 4358595186090