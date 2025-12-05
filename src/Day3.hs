{-# LANGUAGE DerivingVia #-}

module Day3 where

import Control.Applicative (Alternative (some), (<|>))

import Control.Monad.Tardis
import Data.Functor (($>))
import Data.Semigroup
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec, runParser, sepEndBy)
import Text.Megaparsec.Char

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

joltage :: [Battery] -> Joltage
joltage bs = foldMap Joltage $ fmap (uncurry calculatePower) (zip [0 ..] bs)
 where
  l = (length bs) -1
  calculatePower p b = (10 ^ (l -p)) * ((digit . battery) b)


data PowerBank = PowerBank {batteries :: [Battery]} deriving (Eq, Show)

type Past a = Maybe (a, a)
type Future a = Maybe a
type TimeMachine a b = Tardis (Future a) (Past a) b

timeMachine :: (Ord a) => a -> TimeMachine a ()
timeMachine present = do
  sendPresentToTheFuture present
  future <- getFuture
  past <- getPast
  modifyThePast present future past

sendPresentToTheFuture :: (Ord a) => a -> TimeMachine a ()
sendPresentToTheFuture present = modifyBackwards (changeFuture present)
 where
  changeFuture :: (Ord a) => a -> Maybe a -> Maybe a
  changeFuture current Nothing = Just current
  changeFuture current (Just future) = Just (max current future)

modifyThePast :: (Ord a) => a -> Maybe a -> Maybe (a, a) -> TimeMachine a ()
modifyThePast present future past = modifyForwards (const history)
 where
  history = changeHistory present future past
  changeHistory :: (Ord a) => a -> Maybe a -> Maybe (a, a) -> Maybe (a, a)
  changeHistory _ Nothing previous = previous
  changeHistory current (Just next) Nothing = Just (current, next)
  changeHistory current (Just next) (Just previous) = Just (max (current, next) previous)

timeTravel :: (Ord a) => [a] -> TimeMachine a (Past a)
timeTravel = (>> getPast) . mapM_ timeMachine

highestSegment :: (Ord a) => Int -> [a] -> Maybe [a]
highestSegment = const $ fmap (\p -> [fst p, snd p]) . runTimeMachine . timeTravel
 where
  runTimeMachine :: TimeMachine a (Past a) -> Past a
  runTimeMachine = flip evalTardis (Nothing, Nothing)

highestJoltage :: Int -> PowerBank -> Joltage
highestJoltage k = foldMap joltage . (highestSegment k) . batteries

solvePart1 :: [PowerBank] -> Joltage
solvePart1 = foldMap (highestJoltage 2)

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
