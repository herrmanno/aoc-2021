module Day12Spec (spec) where

import Test.Hspec
import Day12 (part1, part2)

spec :: SpecWith ()
spec = do
    describe "day 12" $ do
        it "should solve part 1" $ do
            input <- readFile "input/12.txt"
            part1 input `shouldBe` 4378

        it "should solve part 2" $ do
            input <- readFile "input/12.txt"
            part2 input `shouldBe` 133621