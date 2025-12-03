{-# LANGUAGE QuasiQuotes #-}

module Day2Spec where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Day2
import NeatInterpolation (trimming)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

input =
  [trimming|
  11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124
|]

ranges :: [Range Int]
ranges =
  [ Range 11 22
  , Range 95 115
  , Range 998 1012
  , Range 1188511880 1188511890
  , Range 222220 222224
  , Range 1698522 1698528
  , Range 446443 446449
  , Range 38593856 38593862
  , Range 565653 565659
  , Range 824824821 824824827
  , Range 2121212118 2121212124
  ]

spec :: Spec
spec = describe "Day 2" $ do
  it "can parse input" $
    parse input `shouldBe` Right ranges
  it "can generate all intermediate numbers" $
    range (Range 998 1012) `shouldBe` [998, 999, 1000, 1001, 1002, 1003, 1004, 1005, 1006, 1007, 1008, 1009, 1010, 1011, 1012]

  it "can solve part1" $
    solvePart1 ranges `shouldBe` 1227775554
