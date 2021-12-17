module Day17Spec (spec) where

import Test.Hspec
import Day17 (part1, part2)

spec :: SpecWith ()
spec = do
    describe "day 17" $ do
        it "should solve part 1" $ do
            input <- readFile "input/17.txt"
            part1 input `shouldBe` 12090

        it "should solve part 2" $ do
            input <- readFile "input/17.txt"
            part2 input `shouldBe` 5059