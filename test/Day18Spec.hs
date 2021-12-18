module Day18Spec (spec) where

import Test.Hspec
import Day18 (part1, part2)

spec :: SpecWith ()
spec = do
    describe "day 18" $ do
        it "should solve part 1" $ do
            input <- readFile "input/18.txt"
            part1 input `shouldBe` 4008

        it "should solve part 2" $ do
            input <- readFile "input/18.txt"
            part2 input `shouldBe` 4667