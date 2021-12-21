module Day20Spec (spec) where

import Test.Hspec
import Day20 (part1, part2)

spec :: SpecWith ()
spec = do
    describe "day 20" $ do
        it "should solve part 1" $ do
            input <- readFile "input/20.txt"
            part1 input `shouldBe` 5765

        it "should solve part 2" $ do
            input <- readFile "input/20.txt"
            part2 input `shouldBe` 18509