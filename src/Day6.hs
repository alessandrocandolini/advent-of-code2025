module Day6 where

import Control.Applicative (some, (<|>))
import Data.Functor (($>))
import Data.List (transpose)
import Data.Semigroup (Product (..), Sum (..))
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec, runParser, sepBy1, sepEndBy1)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

data Solution = Solution Integer Integer deriving (Eq, Show)

program :: T.Text -> IO ()
program = print . solve

solve :: T.Text -> Either ParsingError Solution
solve = fmap (Solution <$> solvePart1 <*> solvePart2) . parse

data Operation = Addition | Multiplication deriving (Eq, Show)

data Input = Input [[Integer]] [Operation] deriving (Eq, Show)

zipWithOperation :: Input -> [(Operation, [Integer])]
zipWithOperation (Input rows operations) = zip operations (transpose rows)

applyOperation :: Operation -> [Integer] -> Integer
applyOperation Addition = getSum . foldMap Sum
applyOperation Multiplication = getProduct . foldMap Product

applyOperations :: [(Operation, [Integer])] -> [Integer]
applyOperations = fmap (uncurry applyOperation)

solvePart1 :: Input -> Integer
solvePart1 = sum . applyOperations . zipWithOperation

solvePart2 :: Input -> Integer
solvePart2 = solvePart1

type Parser = Parsec Void T.Text
type ParsingError = ParseErrorBundle T.Text Void

operationParser :: Parser Operation
operationParser =
  char '*' $> Multiplication
    <|> char '+' $> Addition

operationsParser :: Parser [Operation]
operationsParser = sepEndBy1 operationParser hspace1

rowParser :: Parser [Integer]
rowParser = hspace *> (sepBy1 decimal hspace1) <* newline

rowsParser :: Parser [[Integer]]
rowsParser = some rowParser

parser :: Parser Input
parser = Input <$> rowsParser <*> operationsParser

parse :: T.Text -> Either ParsingError Input
parse = runParser parser "input day 6"
