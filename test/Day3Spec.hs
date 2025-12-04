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

  it "highest pair list of two elements" $
    highestPair [1,2] `shouldBe` Just (1,2)

  it "highest pair list of three elements" $
    highestPair [1,2,3] `shouldBe` Just (2,3)

  it "highest pair of a larger list" $
    highestPair [2,9,5,1,2,7,3,7] `shouldBe` Just (9,7)

  it "highest pair is empty for empty list" $
    highestPair ([] :: [Int]) `shouldBe` Nothing

  it "highest pair is empty for a list of one element" $
    highestPair [1] `shouldBe` Nothing

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
