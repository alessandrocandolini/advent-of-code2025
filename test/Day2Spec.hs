{-# LANGUAGE QuasiQuotes #-}

module Day2Spec where

import Day2
import NeatInterpolation (trimming)
import Test.Hspec

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
    allElementsInRange (Range 998 1012) `shouldBe` [998, 999, 1000, 1001, 1002, 1003, 1004, 1005, 1006, 1007, 1008, 1009, 1010, 1011, 1012]

  it "can solve part1" $
    solvePart1 ranges `shouldBe` 1227775554

  it "calculate prefix size candidates" $
    candidatePeriodLengths 10 `shouldBe` [5, 2, 1]

  it "check if a given list is made of a repeated index" $
    periods [1, 2, 3, 1, 2, 3] `shouldBe` [[1, 2, 3]]

  it "check if a given list is made of a repeated index (case where there are multiple prefixes and code should extract the maximal)" $
    periods [1, 2, 1, 2, 1, 2, 1, 2] `shouldBe` [[1, 2, 1, 2], [1, 2]]

  it "check if a given list is made of a repeated element" $
    periods [9, 9, 9] `shouldBe` [[9]]

  it "should not return any period when there are none" $
    periods [1, 2, 3, 1, 2, 3, 1, 2, 3, 4, 1, 2, 3] `shouldBe` []

  it "can solve part2" $
    solvePart2 ranges `shouldBe` 4174379265

  it "solve both parts from input" $
    solve input `shouldBe` Right (Answer 1227775554 4174379265)
