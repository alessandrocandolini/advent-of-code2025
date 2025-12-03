{-# LANGUAGE DerivingVia #-}

module Day3 where

import Control.Applicative (Alternative (some), (<|>))

-- import Control.Monad.Combinators.NonEmpty (some)
import Data.Functor (($>))
import Data.List (group, sortOn, tails)
import Data.Semigroup
import qualified Data.Text as T
import Data.Void (Void)
import Safe (headMay)
import Text.Megaparsec (ParseErrorBundle, Parsec, runParser, sepEndBy)
import Text.Megaparsec.Char
import Witherable (mapMaybe)

data Answer = Answer Joltage deriving (Eq, Show)

program :: T.Text -> IO ()
program = print . solve

solve :: T.Text -> Either ParsingError Answer
solve = fmap (Answer . solvePart1) . parse

newtype Joltage = Joltage Int
  deriving (Eq, Show, Ord, Enum, Bounded, Num) via (Int)
  deriving (Semigroup, Monoid) via (Sum Int)

data Digit = One | Two | Three | Four | Five | Six | Seven | Eight | Nine deriving (Eq)

digit :: Digit -> Int
digit One = 1
digit Two = 2
digit Three = 3
digit Four = 4
digit Five = 5
digit Six = 6
digit Seven = 7
digit Eight = 8
digit Nine = 9

instance Show Digit where
  show = show . digit

instance Ord Digit where
  compare a b = compare (digit a) (digit b)

newtype Battery = Battery {battery :: Digit} deriving (Eq, Show, Ord) via Digit

joltage :: Battery -> Battery -> Joltage
joltage (Battery b1) (Battery b2) = Joltage $ read $ (show b1) ++ (show b2)

data PowerBank = PowerBank {batteries :: [Battery]} deriving (Eq, Show)

combinations :: [a] -> [(a, a)]
combinations = concatMap zipWithTail . tails
 where
  zipWithTail [] = []
  zipWithTail (a : as) = (,) <$> [a] <*> as

uniqueCombinations :: (Eq a) => [a] -> [(a, a)]
uniqueCombinations = combinations . duplicateIfSingleton . mapMaybe headMay . group
 where
  duplicateIfSingleton (a : []) = a : [a]
  duplicateIfSingleton as = as

highestJoltageBatteryPair :: PowerBank -> Maybe (Battery, Battery)
highestJoltageBatteryPair = headMay . sortOn (negate . uncurry joltage) . uniqueCombinations . batteries

highestJoltage :: PowerBank -> Joltage
highestJoltage = foldMap (uncurry joltage) . highestJoltageBatteryPair

solvePart1 :: [PowerBank] -> Joltage
solvePart1 = foldMap highestJoltage

type Parser = Parsec Void T.Text
type ParsingError = ParseErrorBundle T.Text Void

digitParser :: Parser Digit
digitParser =
  char '1' $> One
    <|> char '2' $> Two
    <|> char '3' $> Three
    <|> char '4' $> Four
    <|> char '5' $> Five
    <|> char '6' $> Six
    <|> char '7' $> Seven
    <|> char '8' $> Eight
    <|> char '9' $> Nine

batteryParser :: Parser Battery
batteryParser = fmap Battery digitParser

powerBankParser :: Parser PowerBank
powerBankParser = fmap PowerBank (some batteryParser)

powerBanksParser :: Parser [PowerBank]
powerBanksParser = sepEndBy powerBankParser newline

parse :: T.Text -> Either ParsingError [PowerBank]
parse = runParser powerBanksParser "input day 3"
