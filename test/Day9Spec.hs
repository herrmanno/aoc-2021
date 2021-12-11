module Day9Spec (spec) where

import Test.Hspec
import Day9 (part1, part2)

spec :: SpecWith ()
spec = do
    describe "day 9" $ do
        it "should solve part 1" $ do
            input <- readFile "input/9.txt"
            part1 input `shouldBe` 502

        it "should solve part 2" $ do
            input <- readFile "input/9.txt"
            part2 input `shouldBe` 1330560