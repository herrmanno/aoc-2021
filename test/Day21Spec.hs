module Day21Spec (spec) where

import Test.Hspec
import Day21 (part1, part2)

spec :: SpecWith ()
spec = do
    describe "day 21" $ do
        it "should solve part 1" $ do
            input <- readFile "input/21.txt"
            part1 input `shouldBe` 1196172

        it "should solve part 2" $ do
            input <- readFile "input/21.txt"
            part2 input `shouldBe` 106768284484217