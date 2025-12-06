{-# LANGUAGE QuasiQuotes #-}

module Day3Spec where

import Data.List.NonEmpty
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Day3
import NeatInterpolation (trimming)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

input :: T.Text
input =
  [trimming|
987654321111111
811111111111119
234234234234278
818181911112111
|]

batteries1 :: [Battery]
batteries1 = fmap Battery [Nine, Eight, Seven, Six, Five, Four, Three, Two, One, One, One, One, One, One, One]

batteries2 :: [Battery]
batteries2 = fmap Battery [Eight, One, One, One, One, One, One, One, One, One, One, One, One, One, Nine]

batteries3 :: [Battery]
batteries3 = fmap Battery [Two, Three, Four, Two, Three, Four, Two, Three, Four, Two, Three, Four, Two, Seven, Eight]

batteries4 :: [Battery]
batteries4 = fmap Battery [Eight, One, Eight, One, Eight, One, Nine, One, One, One, One, Two, One, One, One]

powerBank1 = PowerBank batteries1
powerBank2 = PowerBank batteries2
powerBank3 = PowerBank batteries3
powerBank4 = PowerBank batteries4

powerBanks :: [PowerBank]
powerBanks = [powerBank1, powerBank2, powerBank3, powerBank4]

spec :: Spec
spec = describe "Day 3" $ do
  it "parse input" $
    parse input `shouldBe` (Right powerBanks)

  it "joltage" $
    joltage [Battery One, Battery Two, Battery Three] `shouldBe` 123

  it "highest pair list of two elements" $
    maximumSubsequence 2 [1, 2] `shouldBe` Just [1, 2]

  it "highest pair list of three elements" $
    maximumSubsequence 2 [1, 2, 3] `shouldBe` Just [2, 3]

  it "highest pair of a larger list" $
    maximumSubsequence 2 [2, 9, 5, 1, 2, 7, 3, 7] `shouldBe` Just [9, 7]

  it "highest pair is empty for empty list" $
    maximumSubsequence 2 ([] :: [Int]) `shouldBe` Nothing

  it "highest pair is empty for a list of one element" $
    maximumSubsequence 2 [1] `shouldBe` Nothing

  it "highest joltage of powerbank 1" $
    highestJoltage 2 powerBank1 `shouldBe` 98

  it "highest joltage of powerbank 2" $
    highestJoltage 2 powerBank2 `shouldBe` 89

  it "highest joltage of powerbank 3" $
    highestJoltage 2 powerBank3 `shouldBe` 78

  it "highest joltage of powerbank 4" $
    highestJoltage 2 powerBank4 `shouldBe` 92

  it "highest joltage 12 of powerbank 1" $
    highestJoltage 12 powerBank1 `shouldBe` 987654321111

  it "highest joltage 12 of powerbank 2" $
    highestJoltage 12 powerBank2 `shouldBe` 811111111119

  it "highest joltage 12 of powerbank 3" $
    highestJoltage 12 powerBank3 `shouldBe` 434234234278

  it "highest joltage 12 of powerbank 4" $
    highestJoltage 12 powerBank4 `shouldBe` 888911112111

  it "solve part 1" $
    solvePart1 powerBanks `shouldBe` 357

  it "solve part 2" $
    solvePart2 powerBanks `shouldBe` 3121910778619

  it "solve input " $
    solve input `shouldBe` Right (Answer 357 3121910778619)
