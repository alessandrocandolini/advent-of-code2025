{-# LANGUAGE QuasiQuotes #-}

module Day5Spec where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Day5
import NeatInterpolation (trimming)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

input :: T.Text
input =
  [trimming|
3-5
10-14
16-20
12-18

1
5
8
11
17
32
|]

ranges :: (Num a) => [Interval a]
ranges =
  [ ClosedInterval 3 5
  , ClosedInterval 10 14
  , ClosedInterval 16 20
  , ClosedInterval 12 18
  ]

intervalSet :: (Num a) => IntervalSet a
intervalSet = IntervalSet ranges

toCheck :: (Num a) => [a]
toCheck = [1, 5, 8, 11, 17, 32]

exampleInput :: Ingredients
exampleInput = Ingredients intervalSet toCheck

spec :: Spec
spec = describe "Day 5" $ do
  it "can parse input" $
    parse input `shouldBe` (Right exampleInput)

  it "check ingredient that falls into one of the ranges" $
    checkFreshness intervalSet 5 `shouldBe` Fresh

  it "check ingredient that falls into multiple ranges" $
    checkFreshness intervalSet 17 `shouldBe` Fresh

  it "check ingredient that does not fall into any range" $
    checkFreshness intervalSet 8 `shouldBe` Spoiled

  it "solve part 1" $
    solvePart1 exampleInput `shouldBe` 3

  it "nonOverlapping with disjoint intervals" $
    let
      is =
        fromList
          [ ClosedInterval 3 5
          , ClosedInterval 10 20
          ]
     in
      nonOverlapping is `shouldBe` (intervals is)
  it "nonOverlapping with tangential intervals" $
    let
      is =
        fromList
          [ ClosedInterval 3 5
          , ClosedInterval 5 10
          ]
     in
      nonOverlapping is `shouldBe` [ClosedInterval 3 10]

  it "nonOverlapping with lower bound in arbitrary positions" $
    let
      is =
        fromList
          [ ClosedInterval 5 10
          , ClosedInterval 2 3
          , ClosedInterval 1 20
          ]
     in
      nonOverlapping is `shouldBe` [ClosedInterval 1 20]

  it "nonOverlapping overlapping backward" $
    let
      is =
        fromList
          [ ClosedInterval 10 12
          , ClosedInterval 1 4
          , ClosedInterval 3 15
          ]
     in
      nonOverlapping is `shouldBe` [ClosedInterval 1 15]

  it "nonOverlapping" $
    nonOverlapping intervalSet
      `shouldBe` [ ClosedInterval 3 5
                 , ClosedInterval 10 20
                 ]

  it "solve both parts from input" $
    solve input `shouldBe` (Right (Solution 3 14))
