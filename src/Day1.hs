module Day1 where

import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.List (scanl')
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec, runParser, sepEndBy)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

data Answer = Answer {solutionPart1 :: Int, solutionPart2 :: Int} deriving (Eq, Show)

program :: T.Text -> IO ()
program = print . solve

solve :: T.Text -> Either ParsingError Answer
solve = fmap answer . parse
 where
  answer = Answer <$> solvePart1 <*> solvePart2

data Direction = Leftward | Rightward deriving (Eq, Show)

data Rotation = Rotation
  { direction :: Direction
  , distance :: Int
  }
  deriving (Eq, Show)

-- always assume deal [0, dialSize[
dialSize :: Int
dialSize = 100

rotate :: Int -> Rotation -> Int
rotate position rotation = (position + increment rotation) `mod` dialSize
 where
  increment (Rotation Leftward d) = -d
  increment (Rotation Rightward d) = d

dialPositions :: Int -> [Rotation] -> [Int]
dialPositions initial rotations = scanl' rotate initial rotations

solvePart1 :: [Rotation] -> Int
solvePart1 = length . filter ((==) 0) . dialPositions 50

expandRotation :: Rotation -> [Rotation]
expandRotation (Rotation d1 d2) = replicate d2 (Rotation d1 1)

expandRotations :: [Rotation] -> [Rotation]
expandRotations = concatMap expandRotation

solvePart2 :: [Rotation] -> Int
solvePart2 = solvePart1 . expandRotations

type Parser = Parsec Void T.Text
type ParsingError = ParseErrorBundle T.Text Void

rotationParser :: Parser Rotation
rotationParser = Rotation <$> directionParser <*> decimal

directionParser :: Parser Direction
directionParser =
  char 'L' $> Leftward
    <|> char 'R' $> Rightward

rotationsParser :: Parser [Rotation]
rotationsParser = sepEndBy rotationParser newline

parse :: T.Text -> Either ParsingError [Rotation]
parse = runParser rotationsParser "input day 1"
