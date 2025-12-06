module Day4 where

import Control.Applicative (some, (<|>))
import Control.Comonad.Store
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec, runParser, sepEndBy)
import Text.Megaparsec.Char

data Solution = Solution
  { solutionPart1 :: Int
  , solutionPart2 :: Int
  }
  deriving (Eq, Show)

program :: T.Text -> IO ()
program = print . solve

solve :: T.Text -> Either ParsingError Solution
solve = fmap (Solution <$> solvePart1 <*> solvePart1) . parse

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

accessibilityGrid :: Int -> [[Maybe Cell]] -> (Bounds, Grid Cell)
accessibilityGrid threshold cells =
  let
    (bounds, grid) = fromListUnsafe cells
    result = gridOfAccessibleSites threshold grid
   in
    (bounds, result)

solvePart1 :: [[Maybe Cell]] -> Int
solvePart1 cells =
  let
    (bounds, grid) = accessibilityGrid 4 cells
   in
    countAllAccessible bounds grid

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
