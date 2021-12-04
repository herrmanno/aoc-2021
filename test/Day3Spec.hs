module Day3Spec (spec) where

import Test.Hspec
import Day3 (part1, part2)

spec :: SpecWith ()
spec = do
    describe "day 3" $ do
        it "should solve part 1" $ do
            input <- readFile "input/3.txt"
            part1 input `shouldBe` "Just 845186"

        it "should solve part 2" $ do
            input <- readFile "input/3.txt"
            part2 input `shouldBe` "4636702"