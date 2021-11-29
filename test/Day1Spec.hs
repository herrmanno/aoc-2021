module Day1Spec (spec) where

import Test.Hspec
import Day1 (part1, part2)

spec = do
    xdescribe "day 1" $ do
        it "should solve part 1" $
            part1 "" `shouldBe` "1"

        it "should solve part 2" $
            part2 "" `shouldBe` "1"