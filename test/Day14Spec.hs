module Day14Spec (spec) where

import Test.Hspec
import Day14 (part1, part2)

spec :: SpecWith ()
spec = do
    describe "day 14" $ do
        it "should solve part 1" $ do
            input <- readFile "input/14.txt"
            part1 input `shouldBe` 2602

        it "should solve part 2" $ do
            input <- readFile "input/14.txt"
            part2 input `shouldBe` 2942885922173