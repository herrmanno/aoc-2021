module Day6Spec (spec) where

import Test.Hspec
import Day6 (part1, part2)

spec :: SpecWith ()
spec = do
    describe "day 6" $ do
        it "should solve part 1" $ do
            input <- readFile "input/6.txt"
            part1 input `shouldBe` 351188

        it "should solve part 2" $ do
            input <- readFile "input/6.txt"
            part2 input `shouldBe` 1595779846729