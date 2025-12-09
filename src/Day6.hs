module Day6 where

import Control.Applicative (some, (<|>))
import Data.Functor (($>))
import Data.List (transpose)
import Data.Semigroup (Product (..), Sum (..))
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec, runParser, sepEndBy1)
import Text.Megaparsec.Char
import Data.List.Split (splitOn, splitWhen)

data Solution = Solution Integer Integer deriving (Eq, Show)

program :: T.Text -> IO ()
program = print . solve

solve :: T.Text -> Either ParsingError Solution
solve = fmap (Solution <$> solvePart1 <*> solvePart2) . parse

data Operation = Addition | Multiplication deriving (Eq, Show)

data Input = Input [String] [Operation] deriving (Eq, Show)

buildNumbers1 :: [String] -> [[Integer]]
buildNumbers1 =  transpose . fmap unsafeParseNumbers

buildNumbers2 :: [String] -> [[Integer]]
buildNumbers2 = fmap concat . splitWhen null . fmap unsafeParseNumbers . transpose

unsafeParseNumbers :: String -> [Integer]
unsafeParseNumbers = fmap read . filter (not . null) . splitOn " "

zipWithOperation :: [[Integer]] -> [Operation] -> [(Operation, [Integer])]
zipWithOperation numbers operations = zip operations numbers

applyOperation :: Operation -> [Integer] -> Integer
applyOperation Addition = getSum . foldMap Sum
applyOperation Multiplication = getProduct . foldMap Product

applyOperations :: [(Operation, [Integer])] -> [Integer]
applyOperations = fmap (uncurry applyOperation)

solvePart1 :: Input -> Integer
solvePart1 (Input rows operations) = (solveCommond operations . buildNumbers1) rows

solvePart2 :: Input -> Integer
solvePart2 (Input rows operations) = (solveCommond operations . buildNumbers2) rows

solveCommond :: [Operation] -> [[Integer]] -> Integer
solveCommond operations = sum . applyOperations . flip zipWithOperation operations

type Parser = Parsec Void T.Text
type ParsingError = ParseErrorBundle T.Text Void

operationParser :: Parser Operation
operationParser =
  char '*' $> Multiplication
    <|> char '+' $> Addition

operationsParser :: Parser [Operation]
operationsParser = sepEndBy1 operationParser hspace1

rowParser :: Parser String
rowParser = some (digitChar <|> char ' ') <* newline

rowsParser :: Parser [String]
rowsParser = some rowParser

parser :: Parser Input
parser = Input <$> rowsParser <*> operationsParser

parse :: T.Text -> Either ParsingError Input
parse = runParser parser "input day 6"
