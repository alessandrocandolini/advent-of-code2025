module Day2 where

import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec, runParser, sepBy)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)

data Answer = Answer Int Int deriving (Eq, Show)

program :: T.Text -> IO ()
program = print . solve

solve :: T.Text -> Either ParsingError Answer
solve = fmap (Answer <$> solvePart1 <*> solvePart2) . parse

data Range a = Range {from :: a, to :: a} deriving (Eq, Show)

allElementsInRange :: (Enum a) => Range a -> [a]
allElementsInRange r = [from r .. to r]

isRepeatedTwice :: (Eq a) => [a] -> Bool
isRepeatedTwice as = firstHalf ++ firstHalf == as
 where
  firstHalf = take ((length as) `div` 2) as

solvePart1 :: [Range Int] -> Int
solvePart1 = sum . filter areDigitsRepeatedTwice . concatMap allElementsInRange
 where
  areDigitsRepeatedTwice = isRepeatedTwice . show

prefixesOfLengths :: [Int] -> [a] -> [[a]]
prefixesOfLengths sizes as = fmap (`take` as) sizes

-- naive check
hasPeriod :: (Eq a) => [a] -> [a] -> Bool
hasPeriod original period = hasLengthMultiple && coverInitialList
 where
  coverInitialList = original == take (length original) (cycle period)
  hasLengthMultiple = (length original `mod` length period) == 0

candidatePeriodLengths :: Int -> [Int]
candidatePeriodLengths n = [l | l <- [half, half - 1 .. 1], n `mod` l == 0]
 where
  half = n `div` 2

candidatePeriods :: [a] -> [[a]]
candidatePeriods as = prefixesOfLengths (candidatePeriodLengths (length as)) as

periods :: (Eq a) => [a] -> [[a]]
periods as = filter (hasPeriod as) (candidatePeriods as)

isPeriodic :: (Eq a) => [a] -> Bool
isPeriodic = not . null . periods

solvePart2 :: [Range Int] -> Int
solvePart2 = sum . filter isMadeByPeriodicDigits . concatMap allElementsInRange
 where
  isMadeByPeriodicDigits = isPeriodic . show

type Parser = Parsec Void T.Text
type ParsingError = ParseErrorBundle T.Text Void

rangeParser :: Parser (Range Int)
rangeParser = Range <$> (decimal <* char '-') <*> decimal

rangesParser :: Parser [Range Int]
rangesParser = sepBy rangeParser (char ',')

parse :: T.Text -> Either ParsingError [Range Int]
parse = runParser rangesParser "input day 2"
