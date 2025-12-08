{-# LANGUAGE QuasiQuotes #-}
module Day6Spec where

import Day6
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Data.Text as T
import NeatInterpolation (trimming)
import Text.Megaparsec (runParser)
import SpecUtils (shouldBePretty)

input :: T.Text
input = [trimming|
123 328  51 64
 45 64  387 23
  6 98  215 314
*   +   *   +
|]

rows :: [[Integer]]
rows =[[123, 328, 51, 64], [45,64,387,23], [6, 98, 215, 314]]

operationsInput :: T.Text
operationsInput = [trimming|
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

  it "parse full input" $
    parse input `shouldBePretty` Right parsedInput

  it "apply operation addition" $
    applyOperation Addition [1,2,3,4] `shouldBe` 10

  it "apply operation addition" $
    applyOperation Multiplication [1,2,3,4] `shouldBe` 24

  it "apply operations" $
    applyOperations [(Addition, [1,2,3,4]), (Multiplication, [1,2,3,4])] `shouldBe` [10, 24]

  it "solve part 1 " $
    solvePart1 parsedInput `shouldBe` 4277556

  it "Solve both parts from input" $
    solve input `shouldBePretty` Right (Solution 4277556 4277556)

  prop ""
    $ \l -> reverse (reverse l) == (l :: [Int])

