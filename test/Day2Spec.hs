module Day2Spec (spec) where

import Test.Hspec
import Day2 (part1, part2)

spec :: SpecWith ()
spec = do
    describe "day 1" $ do
        it "should solve part 1" $ do
            input <- readFile "input/2.txt"
            part1 input `shouldBe` "1938402"

        it "should solve part 2" $ do
            input <- readFile "input/2.txt"
            part2 input `shouldBe` "1947878632"