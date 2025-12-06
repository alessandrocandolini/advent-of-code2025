{-# LANGUAGE QuasiQuotes #-}

module Day4Spec where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Day4
import NeatInterpolation (trimming)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

input :: T.Text
input =
  [trimming|
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
|]

exampleGrid :: [[Maybe Cell]]
exampleGrid =
  [ [Nothing, Nothing, Just Inaccessible, Just Inaccessible, Nothing, Just Inaccessible, Just Inaccessible, Just Inaccessible, Just Inaccessible, Nothing]
  , [Just Inaccessible, Just Inaccessible, Just Inaccessible, Nothing, Just Inaccessible, Nothing, Just Inaccessible, Nothing, Just Inaccessible, Just Inaccessible]
  , [Just Inaccessible, Just Inaccessible, Just Inaccessible, Just Inaccessible, Just Inaccessible, Nothing, Just Inaccessible, Nothing, Just Inaccessible, Just Inaccessible]
  , [Just Inaccessible, Nothing, Just Inaccessible, Just Inaccessible, Just Inaccessible, Just Inaccessible, Nothing, Nothing, Just Inaccessible, Nothing]
  , [Just Inaccessible, Just Inaccessible, Nothing, Just Inaccessible, Just Inaccessible, Just Inaccessible, Just Inaccessible, Nothing, Just Inaccessible, Just Inaccessible]
  , [Nothing, Just Inaccessible, Just Inaccessible, Just Inaccessible, Just Inaccessible, Just Inaccessible, Just Inaccessible, Just Inaccessible, Nothing, Just Inaccessible]
  , [Nothing, Just Inaccessible, Nothing, Just Inaccessible, Nothing, Just Inaccessible, Nothing, Just Inaccessible, Just Inaccessible, Just Inaccessible]
  , [Just Inaccessible, Nothing, Just Inaccessible, Just Inaccessible, Just Inaccessible, Nothing, Just Inaccessible, Just Inaccessible, Just Inaccessible, Just Inaccessible]
  , [Nothing, Just Inaccessible, Just Inaccessible, Just Inaccessible, Just Inaccessible, Just Inaccessible, Just Inaccessible, Just Inaccessible, Just Inaccessible, Nothing]
  , [Just Inaccessible, Nothing, Just Inaccessible, Nothing, Just Inaccessible, Just Inaccessible, Just Inaccessible, Nothing, Just Inaccessible, Nothing]
  ]

exampleOutput :: T.Text
exampleOutput =
  [trimming|
..xx.xx@x.
x@@.@.@.@@
@@@@@.x.@@
@.@@@@..@.
x@.@@@@.@x
.@@@@@@@.@
.@.@.@.@@@
x.@@@.@@@@
.@@@@@@@@.
x.x.@@@.x.
|]

expectedOutput :: [[Maybe Cell]]
expectedOutput =
  [
    [ Nothing
    , Nothing
    , Just Accessible
    , Just Accessible
    , Nothing
    , Just Accessible
    , Just Accessible
    , Just Inaccessible
    , Just Accessible
    , Nothing
    ]
  ,
    [ Just Accessible
    , Just Inaccessible
    , Just Inaccessible
    , Nothing
    , Just Inaccessible
    , Nothing
    , Just Inaccessible
    , Nothing
    , Just Inaccessible
    , Just Inaccessible
    ]
  ,
    [ Just Inaccessible
    , Just Inaccessible
    , Just Inaccessible
    , Just Inaccessible
    , Just Inaccessible
    , Nothing
    , Just Accessible
    , Nothing
    , Just Inaccessible
    , Just Inaccessible
    ]
  ,
    [ Just Inaccessible
    , Nothing
    , Just Inaccessible
    , Just Inaccessible
    , Just Inaccessible
    , Just Inaccessible
    , Nothing
    , Nothing
    , Just Inaccessible
    , Nothing
    ]
  ,
    [ Just Accessible
    , Just Inaccessible
    , Nothing
    , Just Inaccessible
    , Just Inaccessible
    , Just Inaccessible
    , Just Inaccessible
    , Nothing
    , Just Inaccessible
    , Just Accessible
    ]
  ,
    [ Nothing
    , Just Inaccessible
    , Just Inaccessible
    , Just Inaccessible
    , Just Inaccessible
    , Just Inaccessible
    , Just Inaccessible
    , Just Inaccessible
    , Nothing
    , Just Inaccessible
    ]
  ,
    [ Nothing
    , Just Inaccessible
    , Nothing
    , Just Inaccessible
    , Nothing
    , Just Inaccessible
    , Nothing
    , Just Inaccessible
    , Just Inaccessible
    , Just Inaccessible
    ]
  ,
    [ Just Accessible
    , Nothing
    , Just Inaccessible
    , Just Inaccessible
    , Just Inaccessible
    , Nothing
    , Just Inaccessible
    , Just Inaccessible
    , Just Inaccessible
    , Just Inaccessible
    ]
  ,
    [ Nothing
    , Just Inaccessible
    , Just Inaccessible
    , Just Inaccessible
    , Just Inaccessible
    , Just Inaccessible
    , Just Inaccessible
    , Just Inaccessible
    , Just Inaccessible
    , Nothing
    ]
  ,
    [ Just Accessible
    , Nothing
    , Just Accessible
    , Nothing
    , Just Inaccessible
    , Just Inaccessible
    , Just Inaccessible
    , Nothing
    , Just Accessible
    , Nothing
    ]
  ]

spec :: Spec
spec = describe "Day 4" $ do
  it "can parse input" $
    parse input `shouldBe` Right exampleGrid

  it "calculate bounds" $
    let (_, bounds, _) = solvePart1 defaultThreshold exampleGrid
     in bounds `shouldBe` (Bounds 10 10)

  it "calculate accessibility grid" $
    let (_, bounds, grid) = solvePart1 defaultThreshold exampleGrid
        elements = toList bounds grid
     in elements `shouldBe` expectedOutput

  it "print accessibility grid" $
    let (_, bounds, grid) = solvePart1 defaultThreshold exampleGrid
        result = (T.strip . T.pack) (renderAccessibleCells bounds grid)
     in result `shouldBe` exampleOutput

  it "can solve part 1" $
    let (n, _, _) = solvePart1 defaultThreshold exampleGrid
     in n `shouldBe` 13

  it "solve both parts from input" $
    let solution = fmap fromVerboseSolution (solve input)
     in solution `shouldBe` Right (Solution 13 43)
