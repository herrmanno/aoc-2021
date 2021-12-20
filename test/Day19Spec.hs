module Day19Spec (spec) where

import Test.Hspec
import Day19 (part1, part2)

spec :: SpecWith ()
spec = do
    describe "day 19" $ do
        it "should solve part 1" $ do
            input <- readFile "input/19_test.txt"
            part1 input `shouldBe` 79

        it "should solve part 2" $ do
            input <- readFile "input/19_test.txt"
            part2 input `shouldBe` 3621