{-# LANGUAGE DerivingVia #-}

module Day5 where

import Data.List (sortOn)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec, empty, endBy, runParser, sepEndBy)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

data Solution = Solution Int Integer deriving (Eq, Show)

data Interval a = ClosedInterval {lowerBound :: a, upperBound :: a} deriving (Eq, Show)

interval :: (Ord a) => a -> a -> Maybe (Interval a)
interval lower upper
  | lower <= upper = Just (ClosedInterval lower upper)
  | otherwise = Nothing

isMember :: (Ord a) => a -> Interval a -> Bool
isMember a (ClosedInterval lower upper)
  | a >= lower && a <= upper = True
  | otherwise = False

data IntervalSet a = IntervalSet {intervals :: [Interval a]} deriving (Eq, Show)

fromList :: [Interval a] -> IntervalSet a
fromList = IntervalSet

isMemberOfAtLeastOne :: (Ord a) => a -> IntervalSet a -> Bool
isMemberOfAtLeastOne a = any (isMember a) . intervals

nonOverlapping :: (Ord a) => IntervalSet a -> [Interval a]
nonOverlapping =
  reverse . foldl' merge [] . sortOn lowerBound . intervals
 where
  merge [] i = [i]
  merge (a : as) i
    | upperBound a < lowerBound i = i : a : as
    | otherwise = ClosedInterval (lowerBound a) (max (upperBound a) (upperBound i)) : as

size :: (Num n, Integral a) => Interval a -> n
size (ClosedInterval lower upper) = fromIntegral (upper - lower + 1)

newtype Ingredient = Ingredient Int deriving (Eq, Show, Ord, Bounded, Enum, Num, Real, Integral) via Int

data Freshness = Fresh | Spoiled deriving (Eq, Show)

data Ingredients = Ingredients
  { freshRanges :: (IntervalSet Ingredient)
  , available :: [Ingredient]
  }
  deriving (Eq, Show)

program :: T.Text -> IO ()
program = print . solve

solve :: T.Text -> Either ParsingError Solution
solve = fmap (Solution <$> solvePart1 <*> solvePart2) . parse

checkFreshness :: IntervalSet Ingredient -> Ingredient -> Freshness
checkFreshness s i = if isMemberOfAtLeastOne i s then Fresh else Spoiled

solvePart1 :: Ingredients -> Int
solvePart1 (Ingredients rs as) = (length . filter ((==) Fresh) . fmap (checkFreshness rs)) as

solvePart2 :: Ingredients -> Integer
solvePart2 = sum . fmap size . nonOverlapping . freshRanges

type Parser = Parsec Void T.Text
type ParsingError = ParseErrorBundle T.Text Void

ingredientParser :: Parser Ingredient
ingredientParser = fmap Ingredient decimal

mapMaybeP :: (a -> Maybe b) -> Parser a -> Parser b
mapMaybeP f p = p >>= \a -> maybe empty pure (f a)

intervalParser :: (Ord a) => Parser (a, a) -> Parser (Interval a)
intervalParser = mapMaybeP (uncurry interval)

ingredientPairParser :: Parser (Ingredient, Ingredient)
ingredientPairParser = (,) <$> (ingredientParser <* char '-') <*> ingredientParser

ingredientIntervalParser :: Parser (Interval Ingredient)
ingredientIntervalParser = intervalParser ingredientPairParser

ingredientIntervalsParser :: Parser (IntervalSet Ingredient)
ingredientIntervalsParser = fromList <$> endBy ingredientIntervalParser newline

availableIngredientsParser :: Parser [Ingredient]
availableIngredientsParser = sepEndBy ingredientParser newline

ingredientsDatabaseParser :: Parser Ingredients
ingredientsDatabaseParser =
  Ingredients
    <$> (ingredientIntervalsParser <* newline)
    <*> availableIngredientsParser

parse :: T.Text -> Either ParsingError Ingredients
parse = runParser ingredientsDatabaseParser "input day 5"
