{-# LANGUAGE QuasiQuotes #-}

module Day4Spec where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Day4
import Day4 (accessibilityGrid)
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

grid :: [[Maybe Cell]]
grid =
  [ [Nothing, Nothing, Just RollsOfPaper, Just RollsOfPaper, Nothing, Just RollsOfPaper, Just RollsOfPaper, Just RollsOfPaper, Just RollsOfPaper, Nothing]
  , [Just RollsOfPaper, Just RollsOfPaper, Just RollsOfPaper, Nothing, Just RollsOfPaper, Nothing, Just RollsOfPaper, Nothing, Just RollsOfPaper, Just RollsOfPaper]
  , [Just RollsOfPaper, Just RollsOfPaper, Just RollsOfPaper, Just RollsOfPaper, Just RollsOfPaper, Nothing, Just RollsOfPaper, Nothing, Just RollsOfPaper, Just RollsOfPaper]
  , [Just RollsOfPaper, Nothing, Just RollsOfPaper, Just RollsOfPaper, Just RollsOfPaper, Just RollsOfPaper, Nothing, Nothing, Just RollsOfPaper, Nothing]
  , [Just RollsOfPaper, Just RollsOfPaper, Nothing, Just RollsOfPaper, Just RollsOfPaper, Just RollsOfPaper, Just RollsOfPaper, Nothing, Just RollsOfPaper, Just RollsOfPaper]
  , [Nothing, Just RollsOfPaper, Just RollsOfPaper, Just RollsOfPaper, Just RollsOfPaper, Just RollsOfPaper, Just RollsOfPaper, Just RollsOfPaper, Nothing, Just RollsOfPaper]
  , [Nothing, Just RollsOfPaper, Nothing, Just RollsOfPaper, Nothing, Just RollsOfPaper, Nothing, Just RollsOfPaper, Just RollsOfPaper, Just RollsOfPaper]
  , [Just RollsOfPaper, Nothing, Just RollsOfPaper, Just RollsOfPaper, Just RollsOfPaper, Nothing, Just RollsOfPaper, Just RollsOfPaper, Just RollsOfPaper, Just RollsOfPaper]
  , [Nothing, Just RollsOfPaper, Just RollsOfPaper, Just RollsOfPaper, Just RollsOfPaper, Just RollsOfPaper, Just RollsOfPaper, Just RollsOfPaper, Just RollsOfPaper, Nothing]
  , [Just RollsOfPaper, Nothing, Just RollsOfPaper, Nothing, Just RollsOfPaper, Just RollsOfPaper, Just RollsOfPaper, Nothing, Just RollsOfPaper, Nothing]
  ]

renderedOutput :: T.Text
renderedOutput =
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

expectedOutput :: [[Maybe Accessibility]]
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

displayAccessibility :: Maybe Accessibility -> Char
displayAccessibility (Just Accessible) = 'x'
displayAccessibility (Just Inaccessible) = '@'
displayAccessibility Nothing = '.'

spec :: Spec
spec = describe "Day 4" $ do
  it "can parse input" $
    parse input `shouldBe` Right grid

  it "calculate bounds" $
    let expectedBounds = Bounds 10 10
        (bs, _) = accessibilityGrid 4 grid
     in bs `shouldBe` expectedBounds

  it "calculate accessibility grid" $
    let
      (bs, g) = accessibilityGrid 4 grid
      elements = toList bs g
     in
      elements `shouldBe` expectedOutput

  it "print accessibility grid" $
    let
      (bs, g) = accessibilityGrid 4 grid
      rows = unlines (fmap (fmap displayAccessibility) (toList bs g))
     in
      T.strip (T.pack rows) `shouldBe` renderedOutput

  it "can solve part 1" $
    solvePart1 grid `shouldBe` 13

  it "solve both parts from input" $
    solve input `shouldBe` Right (Solution 13 13)
