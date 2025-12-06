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

data Answer = Answer
  { answerPart1 :: Joltage
  , answerPart2 :: Joltage
  }
  deriving (Eq, Show)

program :: T.Text -> IO ()
program = print . solve

solve :: T.Text -> Either ParsingError Answer
solve = fmap (Answer <$> solvePart1 <*> solvePart2) . parse

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
  l = (length bs) - 1
  calculatePower p b = (10 ^ (l - p)) * ((digit . battery) b)

data PowerBank = PowerBank {batteries :: [Battery]} deriving (Eq, Show)

type Past a = Maybe [a]
type Future a = [Maybe [a]]
type TimeMachine a b = Tardis (Future a) (Past a) b

timeMachine :: (Ord a) => Int -> a -> TimeMachine a ()
timeMachine k present = do
  sendPresentToTheFuture k present
  future <- getFuture
  past <- getPast
  modifyThePast k past present future

sendPresentToTheFuture :: (Ord a) => Int -> a -> TimeMachine a ()
sendPresentToTheFuture k present = modifyBackwards (changeFuture present)
 where
  changeFuture :: (Ord a) => a -> Future a -> Future a
  changeFuture a future = [dpAt l | l <- [0 .. k - 1]]
   where
    dpAt 0 = Just []
    dpAt l =
      let keep = future !! l
          prev = future !! (l - 1)
          cand = (a :) <$> prev
       in betterCandidate keep cand

betterCandidate :: (Ord a) => Maybe [a] -> Maybe [a] -> Maybe [a]
betterCandidate Nothing y = y
betterCandidate x Nothing = x
betterCandidate (Just xs) (Just ys) = Just (max xs ys)

modifyThePast :: (Ord a) => Int -> Past a -> a -> Future a -> TimeMachine a ()
modifyThePast k past present future = modifyForwards (const history)
 where
  segment = future !! (k - 1)
  withReplacedPresent = (present :) <$> segment
  history = betterCandidate past withReplacedPresent

timeTravel :: (Ord a) => Int -> [a] -> TimeMachine a (Past a)
timeTravel k = (>> getPast) . mapM_ (timeMachine k)

maximumSubsequence :: (Ord a) => Int -> [a] -> Maybe [a]
maximumSubsequence = withLengthChecks $ \k -> runTimeMachine k . timeTravel k
 where
  initialFuture :: Int -> Future a
  initialFuture k = Just [] : replicate (k - 1) Nothing
  runTimeMachine :: Int -> TimeMachine a (Past a) -> Past a
  runTimeMachine k = flip evalTardis (initialFuture k, Nothing)

withLengthChecks :: (Int -> [a] -> Maybe [a]) -> Int -> [a] -> Maybe [a]
withLengthChecks f k as
  | k <= 0 = Just []
  | otherwise = f k as

highestJoltage :: Int -> PowerBank -> Joltage
highestJoltage k = foldMap joltage . (maximumSubsequence k) . batteries

solvePart1 :: [PowerBank] -> Joltage
solvePart1 = foldMap (highestJoltage 2)

solvePart2 :: [PowerBank] -> Joltage
solvePart2 = foldMap (highestJoltage 12)

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
