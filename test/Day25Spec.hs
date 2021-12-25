module Day25Spec (spec) where

import Test.Hspec
import Day25 (part1)

spec :: SpecWith ()
spec = do
    describe "day 25" $ do
        it "should solve part 1" $ do
            input <- readFile "input/25.txt"
            part1 input `shouldBe` 471