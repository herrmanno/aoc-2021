module Day23Spec (spec) where

import Test.Hspec
import Day23 (part1, part2)

spec :: SpecWith ()
spec = do
    describe "day 23" $ do
        it "should solve part 1" $ do
            input <- readFile "input/23.txt"
            part1 input `shouldBe` Just 15322

        it "should solve part 2" $ do
            input <- readFile "input/23.txt"
            part2 input `shouldBe` Just 56324