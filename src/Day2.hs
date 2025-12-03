module Day2 where
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Megaparsec (Parsec, ParseErrorBundle, sepBy, runParser)
import Data.Void (Void)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (char)

data Answer = Answer Int deriving (Eq, Show)

program :: T.Text -> IO ()
program = print . solve

solve :: T.Text -> Either ParsingError Answer
solve = fmap (Answer . solvePart1) . parse

data Range a = Range { from :: a , to :: a } deriving (Eq, Show)

range :: Enum a => Range a -> [a]
range r = [from r .. to r]

isRepeatedTwice :: Int -> Bool
isRepeatedTwice = areTheSame . splitInHalf . show where
  splitInHalf as = splitAt (length as `div` 2) as
  areTheSame (s1, s2) = s1 == s2

solvePart1 :: [Range Int] -> Int
solvePart1 = sum . filter isRepeatedTwice . concatMap range

type Parser = Parsec Void T.Text
type ParsingError = ParseErrorBundle T.Text Void

rangeParser :: Parser (Range Int)
rangeParser = Range <$> (decimal <* char '-') <*> decimal

rangesParser :: Parser [Range Int]
rangesParser = sepBy rangeParser (char ',')

parse :: T.Text -> Either ParsingError [Range Int]
parse = runParser rangesParser "input day 2"
