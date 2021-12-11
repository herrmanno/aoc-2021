module Day5Spec (spec) where

import Test.Hspec
import Day5 (part1, part2)

spec :: SpecWith ()
spec = do
    describe "day 5" $ do
        it "should solve part 1" $ do
            input <- readFile "input/5.txt"
            part1 input `shouldBe` 5608

        it "should solve part 2" $ do
            input <- readFile "input/5.txt"
            part2 input `shouldBe` 20299