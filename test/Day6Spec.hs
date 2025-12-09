{-# LANGUAGE QuasiQuotes #-}

module Day6Spec where

import qualified Data.Text as T
import Day6
import NeatInterpolation (text, trimming)
import SpecUtils (shouldBePretty)
import Test.Hspec
import Test.Hspec.QuickCheck
import Text.Megaparsec (runParser)


input :: T.Text
input = T.unlines $ fmap T.pack (rows ++ ["*   +   *   +"])

rows :: [String]
rows =
  [ "123 328  51 64 "
  , " 45 64  387 23 "
  , "  6 98  215 314"
  ]

operationsInput :: T.Text
operationsInput =
  [trimming|
*   +   *   +
|]

operations :: [Operation]
operations = [Multiplication, Addition, Multiplication, Addition]

parsedInput :: Input
parsedInput = Input rows operations

spec :: Spec
spec = describe "Day 5" $ do
  it "parse operations" $
    runParser operationsParser "input day 6" operationsInput `shouldBe` (Right operations)

  it "rows parser" $
    runParser rowsParser "input day 6" input `shouldBePretty` (Right rows)

  it "build numbers from rows part 1 " $
    buildNumbers1 rows `shouldBe` [[123, 45, 6], [328, 64, 98], [51, 387, 215], [64, 23, 314]]

  it "build numbers from rows part 1 " $
    buildNumbers2 rows `shouldBe` [[1, 24, 356], [369, 248, 8], [32, 581, 175], [623, 431, 4]]

  it "parse full input" $
    parse input `shouldBePretty` Right parsedInput

  it "apply operation addition" $
    applyOperation Addition [1, 2, 3, 4] `shouldBe` 10

  it "apply operation addition" $
    applyOperation Multiplication [1, 2, 3, 4] `shouldBe` 24

  it "apply operations" $
    applyOperations [(Addition, [1, 2, 3, 4]), (Multiplication, [1, 2, 3, 4])] `shouldBe` [10, 24]

  it "solve part 1 " $
    solvePart1 parsedInput `shouldBe` 4277556

  it "Solve both parts from input" $
    solve input `shouldBePretty` Right (Solution 4277556 3263827)

  prop "" $
    \l -> reverse (reverse l) == (l :: [Int])
