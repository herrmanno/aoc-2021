module Day22Spec (spec) where

import Test.Hspec
import Day22 (part1, part2)

spec :: SpecWith ()
spec = do
    describe "day 22" $ do
        it "should solve part 1" $ do
            input <- readFile "input/22.txt"
            part1 input `shouldBe` 588120

        it "should solve part 2" $ do
            input <- readFile "input/22.txt"
            part2 input `shouldBe` 1134088247046731