{-# LANGUAGE QuasiQuotes #-}

module Day3Spec where

import Data.List.NonEmpty
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Day3
import Day3 (highestJoltageBatteryPair)
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

  it "combinations of a single element is empty" $
    combinations [1] `shouldBe` []

  it "combinations of a list of two  elements is the pair of elements" $
    combinations [1, 2] `shouldBe` [(1, 2)]

  it "expand all combinations, in the order of the initial list" $
    combinations [1, 2, 3] `shouldBe` [(1, 2), (1, 3), (2, 3)]

  it "expand all combinations, in the order of the initial list" $
    combinations [1, 2, 3, 4, 5] `shouldBe` [(1, 2), (1, 3), (1, 4), (1, 5), (2, 3), (2, 4), (2, 5), (3, 4), (3, 5), (4, 5)]

  it "unique combinations with different elements" $
    uniqueCombinations [1, 2, 3, 1, 1] `shouldBe` [(1, 2), (1, 3), (1, 1), (2, 3), (2, 1), (3, 1)]

  it "unique combinations in case of repeated digits" $
    uniqueCombinations [1, 1, 1, 1] `shouldBe` [(1, 1)]

  prop "combinations and uniqueCombinations produce the same list highest pair" $
    1 `shouldBe` 1

  it "highest joltage pair" $
    highestJoltageBatteryPair powerBank1 `shouldBe` Just ((Battery Nine, Battery Eight))

  it "highest joltage of powerbank 1" $
    highestJoltage powerBank1 `shouldBe` 98

  it "highest joltage of powerbank 2" $
    highestJoltage powerBank2 `shouldBe` 89

  it "highest joltage of powerbank 3" $
    highestJoltage powerBank3 `shouldBe` 78

  it "highest joltage of powerbank 4" $
    highestJoltage powerBank4 `shouldBe` 92
  it "solve part 1" $
    solvePart1 powerBanks `shouldBe` 357
