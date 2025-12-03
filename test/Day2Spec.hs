{-# LANGUAGE QuasiQuotes #-}

module Day2Spec where

import Data.List (inits, singleton)
import qualified Data.Text as T
import Day2
import NeatInterpolation (trimming)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((==>))

input :: T.Text
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

allPrefixes = drop 1 . reverse . drop 1 . inits

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

  it "periods of a list that has only one period" $
    periods [1, 2, 3, 1, 2, 3] `shouldBe` [[1, 2, 3]]

  it "periods of a list that has multiple periods, sorted by length in descending order" $
    periods [1, 2, 1, 2, 1, 2, 1, 2] `shouldBe` [[1, 2, 1, 2], [1, 2]]

  prop "extract periods of a list obtained by repeating the same element a prime number of times" $
    \a n ->
      let
        isPrime k = if k > 1 then null [x | x <- [2 .. k - 1], k `mod` x == 0] else False
       in
        n > 1 && isPrime n ==> periods (replicate n a) `shouldBe` ([(singleton a)] :: [[Int]])

  it "periods of a list of only one element should be empty (ie, we define periods to be of size < of the length of the list)" $
    periods [0] `shouldBe` []

  it "periods of lists with partially repeated pattern" $
    periods [2, 0, 2] `shouldBe` []

  it "hasPeriod of partially repeated patterns" $
    hasPeriod [14, 0] [14, 0, 14] `shouldBe` False

  it "allpredixes helper function generates all prefixes of a non-empty list, excluding the empty list and the list itself, in reverse order" $
    allPrefixes [1, 2, 3, 1, 2, 3] `shouldBe` [[1, 2, 3, 1, 2], [1, 2, 3, 1], [1, 2, 3], [1, 2], [1]]

  it "allprefixes helper function with negative numbers" $
    allPrefixes [-2, 0, -2] `shouldBe` [[-2, 0], [-2]]

  prop "the version of periods that generates candidates returns the same results as a function that naively brute force tries all possible prefixes" $
    \as ->
      (not . null) (as :: [Int])
        ==> filter (hasPeriod as) (allPrefixes as)
        `shouldBe` periods as

  it "should not return any period when there are none" $
    periods [1, 2, 3, 1, 2, 3, 1, 2, 3, 4, 1, 2, 3] `shouldBe` []

  it "can solve part2" $
    solvePart2 ranges `shouldBe` 4174379265

  it "solve both parts from input" $
    solve input `shouldBe` Right (Answer 1227775554 4174379265)
