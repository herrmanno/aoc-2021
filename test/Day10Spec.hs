module Day10Spec (spec) where

import Test.Hspec
import Day10 (part1, part2)

spec :: SpecWith ()
spec = do
    describe "day 10" $ do
        it "should solve part 1" $ do
            input <- readFile "input/10.txt"
            part1 input `shouldBe` 321237

        it "should solve part 2" $ do
            input <- readFile "input/10.txt"
            part2 input `shouldBe` 2360030859