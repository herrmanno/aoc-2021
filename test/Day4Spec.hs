module Day4Spec (spec) where

import Test.Hspec
import Day4 (part1, part2)

spec :: SpecWith ()
spec = do
    describe "day 4" $ do
        it "should solve part 1" $ do
            input <- readFile "input/4.txt"
            part1 input `shouldBe` 58838

        it "should solve part 2" $ do
            input <- readFile "input/4.txt"
            part2 input `shouldBe` 6256