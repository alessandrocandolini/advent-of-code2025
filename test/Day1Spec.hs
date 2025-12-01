{-# LANGUAGE QuasiQuotes #-}

module Day1Spec where

import qualified Data.Text as T
import Day1
import NeatInterpolation (trimming)
import Test.Hspec
import Text.Megaparsec (runParser)

input :: T.Text
input =
  [trimming|
      L68
      L30
      R48
      L5
      R60
      L55
      L1
      L99
      R14
      L82
|]

rotations :: [Rotation]
rotations =
  [ Rotation Leftward 68
  , Rotation Leftward 30
  , Rotation Rightward 48
  , Rotation Leftward 5
  , Rotation Rightward 60
  , Rotation Leftward 55
  , Rotation Leftward 1
  , Rotation Leftward 99
  , Rotation Rightward 14
  , Rotation Leftward 82
  ]

spec :: Spec
spec = describe "Day 1" $ do
  it "can parse single rotation" $
    runParser rotationParser "input" "L68" `shouldBe` (Right (Rotation Leftward 68))

  it "can parse an input with no newline at the end" $
    parse "L68\nL30" `shouldBe` (Right [Rotation Leftward 68, Rotation Leftward 30])

  it "can parse an input with newline at the end" $
    parse "L68\nL30\n" `shouldBe` (Right [Rotation Leftward 68, Rotation Leftward 30])

  it "can parse example input correctly" $
    parse input `shouldBe` (Right rotations)

  it "can rotate Rightward when the rotation is within the range" $
    rotate 11 (Rotation Rightward 8) `shouldBe` 19

  it "can rotate Leftward when the rotation is within the range" $
    rotate 19 (Rotation Leftward 19) `shouldBe` 0

  it "can rotate Rightward when the rotation is out of range" $
    rotate 95 (Rotation Rightward 5) `shouldBe` 0

  it "can rotate Leftward when the rotation is within the range" $
    rotate 5 (Rotation Leftward 10) `shouldBe` 95

  it "can unfold all dial's positions" $
    dialPositions 50 rotations
      `shouldBe` [ 50
                 , 82
                 , 52
                 , 0
                 , 95
                 , 55
                 , 0
                 , 99
                 , 0
                 , 14
                 , 32
                 ]
  it "can solve part1 on example input" $
    solvePart1 rotations `shouldBe` 3

  it "can expand rotations to account for intermediate dial's positions" $
    let
      smallRotations = [Rotation Leftward 5, Rotation Rightward 3]
     in
      dialPositions 2 (expandRotations smallRotations) `shouldBe` [2, 1, 0, 99, 98, 97, 98, 99, 0]

  it "can solve part2 on example input" $
    solvePart2 rotations `shouldBe` 6

  it "can solve part1 and part2 parsing example input" $
    solve input `shouldBe` Right (Answer 3 6)
