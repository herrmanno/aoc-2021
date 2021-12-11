module Day11Spec (spec) where

import Test.Hspec
import Day11 (part1, part2)

spec :: SpecWith ()
spec = do
    describe "day 11" $ do
        it "should solve part 1" $ do
            input <- readFile "input/11.txt"
            part1 input `shouldBe` 1717

        it "should solve part 2" $ do
            input <- readFile "input/11.txt"
            part2 input `shouldBe` 476