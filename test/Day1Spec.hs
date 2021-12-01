module Day1Spec (spec) where

import Test.Hspec
import Day1 (part1, part2)

spec :: SpecWith ()
spec = do
    describe "day 1" $ do
        it "should solve part 1" $ do
            input <- readFile "input/1.txt"
            part1 input `shouldBe` "1121"

        it "should solve part 2" $ do
            input <- readFile "input/1.txt"
            part2 input `shouldBe` "1065"