module Day8Spec (spec) where

import Test.Hspec
import Day8 (part1, part2)

spec :: SpecWith ()
spec = do
    describe "day 8" $ do
        it "should solve part 1" $ do
            input <- readFile "input/8.txt"
            part1 input `shouldBe` 412

        it "should solve part 2" $ do
            input <- readFile "input/8.txt"
            part2 input `shouldBe` 978171