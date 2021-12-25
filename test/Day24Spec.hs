module Day24Spec (spec) where

import Test.Hspec
import Day24 (part1, part2)

spec :: SpecWith ()
spec = do
    describe "day 24" $ do
        it "should solve part 1" $ do
            input <- readFile "input/24.txt"
            part1 input `shouldBe` 99799212949967

        it "should solve part 2" $ do
            input <- readFile "input/24.txt"
            part2 input `shouldBe` 34198111816311