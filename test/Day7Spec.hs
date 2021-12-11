module Day7Spec (spec) where

import Test.Hspec
import Day7 (part1, part2)

spec :: SpecWith ()
spec = do
    describe "day 7" $ do
        it "should solve part 1" $ do
            input <- readFile "input/7.txt"
            part1 input `shouldBe` 336131

        it "should solve part 2" $ do
            input <- readFile "input/7.txt"
            part2 input `shouldBe` 92676646