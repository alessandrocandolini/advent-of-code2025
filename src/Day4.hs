{-# LANGUAGE QuasiQuotes #-}

module Day4 where

import Args (Verbosity (..))
import Control.Applicative (some, (<|>))
import Control.Comonad.Store
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Data.Void (Void)
import NeatInterpolation (text)
import Text.Megaparsec (ParseErrorBundle, Parsec, runParser, sepEndBy)
import Text.Megaparsec.Char

-- helpers
tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- program
program :: Verbosity -> T.Text -> IO ()
program Quiet = T.putStrLn . either tshow renderSolution . fmap fromVerboseSolution . solve
program Verbose = T.putStrLn . either tshow renderVerboseSolution . solve

defaultThreshold :: Int
defaultThreshold = 4

solve :: T.Text -> Either ParsingError VerboseSolution
solve = fmap (VerboseSolution <$> solvePart1 defaultThreshold <*> solvePart2 defaultThreshold) . parse

data VerboseSolution = VerboseSolution
  { verboseSolution1 :: (Int, Bounds, Grid Cell)
  , verboseSolution2 :: (Int, Bounds, [(Int, Grid Cell)])
  }

data Solution = Solution {solution1 :: Int, solution2 :: Int} deriving (Eq, Show)

fromVerboseSolution :: VerboseSolution -> Solution
fromVerboseSolution = Solution <$> (fst3 . verboseSolution1) <*> (fst3 . verboseSolution2) where

renderSolution :: Solution -> T.Text
renderSolution solution =
  [text|
            ***** PART 1 *****
            solution: ${displayCount1}

            ***** PART 2 *****
            solution: ${displayCount2}
            |]
 where
  displayCount1 = (tshow . solution1) solution
  displayCount2 = (tshow . solution2) solution

renderVerboseSolution :: VerboseSolution -> T.Text
renderVerboseSolution verboseSolution =
  [text|
            ***** PART 1 *****
            solution: ${displayCount1}
            grid:
            ${displayGrid1}

            ***** PART 2 *****
            solution: ${displayCount2}
            |]
 where
  (count1, bounds1, grid1) = verboseSolution1 verboseSolution
  displayCount1 = tshow count1
  displayCount2 = (tshow . fst3 . verboseSolution2) verboseSolution
  displayGrid1 = T.pack (renderAccessibleCells bounds1 grid1)

data Cell = Accessible | Inaccessible deriving (Eq, Show)

renderCell :: Maybe Cell -> Char
renderCell (Just Accessible) = 'x'
renderCell (Just Inaccessible) = '@'
renderCell Nothing = '.'

type Coord = (Int, Int)
type Grid a = Store Coord (Maybe a)

data Bounds = Bounds {height :: Int, width :: Int} deriving (Eq, Show)

rowCoords :: Bounds -> Int -> [Coord]
rowCoords b i = [(i, j) | j <- [0 .. width b - 1]]

allRows :: Bounds -> [[Coord]]
allRows b = [rowCoords b i | i <- [0 .. height b - 1]]

-- assume input nested list is "rectangular" and empty grid is modelled as [[]] ([] will crash the program)
fromListUnsafe :: [[Maybe a]] -> (Bounds, Grid a)
fromListUnsafe rows = (bs, store access (0, 0))
 where
  h = length rows
  w = length (head rows)
  bs = Bounds h w
  underlying = V.fromList (concat rows)
  access (i, j)
    | 0 <= i && i < h, 0 <= j && j < w = underlying V.! (i * w + j)
    | otherwise = Nothing

north, south, west, east, northEast, northWest, southEast, southWest :: Coord -> Coord
north (i, j) = (i - 1, j)
south (i, j) = (i + 1, j)
west (i, j) = (i, j - 1)
east (i, j) = (i, j + 1)
northEast = east . north
northWest = west . north
southEast = east . south
southWest = west . south

movements :: [Coord -> Coord]
movements = [north, northEast, east, southEast, south, southWest, west, northWest]

applyMovements :: [Coord -> Coord] -> Coord -> [Coord]
applyMovements allMovements position = fmap ($ position) allMovements

neighbourhood :: Grid a -> [Maybe a]
neighbourhood = experiment (applyMovements movements)

neighbourhoodCheck :: ([Maybe a] -> b) -> Grid a -> Maybe b
neighbourhoodCheck check grid = fmap (const $ check $ neighbourhood grid) (extract grid)

isSiteAccessibleFromNeighbourhood :: Int -> Grid a -> Maybe Cell
isSiteAccessibleFromNeighbourhood threshold = neighbourhoodCheck (isAccessible threshold)

gridOfAccessibleSites :: Int -> Grid a -> Grid Cell
gridOfAccessibleSites threshold = extend (isSiteAccessibleFromNeighbourhood threshold)

isAccessible :: Int -> [Maybe a] -> Cell
isAccessible threshold = enoughFreeSpace . length . filter isJust
 where
  enoughFreeSpace numberOfOccupiedCells
    | numberOfOccupiedCells < threshold = Accessible
    | otherwise = Inaccessible

toList :: Bounds -> Grid a -> [[Maybe a]]
toList b g = fmap (\row -> experiment (const row) g) (allRows b)

countAllAccessible :: Bounds -> Grid Cell -> Int
countAllAccessible b = length . filter (== Just Accessible) . concat . toList b

renderGrid :: (Maybe a -> Char) -> Bounds -> Grid a -> String
renderGrid toChar bounds grid = unlines (fmap (fmap toChar) (toList bounds grid))

renderAccessibleCells :: Bounds -> Grid Cell -> String
renderAccessibleCells = renderGrid renderCell

solvePart1 :: Int -> [[Maybe Cell]] -> (Int, Bounds, Grid Cell)
solvePart1 threshold cells =
  let
    (bounds, grid) = fromListUnsafe cells
    result = gridOfAccessibleSites threshold grid
    count = countAllAccessible bounds result
   in
    (count, bounds, result)

countAllAccessibleEachStep :: Bounds -> [Grid Cell] -> [(Int, Grid Cell)]
countAllAccessibleEachStep bounds = fmap (\grid -> (countAllAccessible bounds grid, grid))

solvePart2 :: Int -> [[Maybe Cell]] -> (Int, Bounds, [(Int, Grid Cell)])
solvePart2 threshold cells =
  let
    (count, bounds, grid) = solvePart1 threshold cells
   in
    (count, bounds, [(count, grid)])

type Parser = Parsec Void T.Text
type ParsingError = ParseErrorBundle T.Text Void

cellParser :: Parser (Maybe Cell)
cellParser =
  char '@' $> Just Inaccessible
    <|> char '.' $> Nothing

rowParser :: Parser [Maybe Cell]
rowParser = some cellParser

gridParser :: Parser [[Maybe Cell]]
gridParser = sepEndBy rowParser newline

parse :: T.Text -> Either ParsingError [[Maybe Cell]]
parse = runParser gridParser "input day 4"
