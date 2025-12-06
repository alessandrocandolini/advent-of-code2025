{-# LANGUAGE QuasiQuotes #-}

module Day4 where

import Args (Verbosity (..))
import Control.Applicative (some, (<|>))
import Control.Comonad.Store
import Data.List (unfoldr)
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

-- render solution
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
fromVerboseSolution = Solution <$> (fst3 . verboseSolution1) <*> (fst3 . verboseSolution2)
 where
  fst3 :: (a, b, c) -> a
  fst3 (a, _, _) = a

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
renderVerboseSolution (VerboseSolution (res1, bounds1, grid1) (res2, bounds2, grids2)) =
  [text|
            ***** PART 1 *****
            grid:
            ${textGrid1}

            ***** PART 2 *****
            evolution:
            ${textGrid2}

            ***** Summary of the solutions *****
            solution part 1: ${textSolution1}
            solution part 2: ${textSolution2}
            |]
 where
  textSolution1 = tshow res1
  textSolution2 = tshow res2
  textGrid1 = T.pack (renderAccessibleCells bounds1 grid1)
  textOneStep (c, g) =
    let
      textCount = tshow c
      textGrid = T.pack (renderAccessibleCells bounds2 g)
     in
      [text|
      Remove ${textCount} rolls of paper:
      ${textGrid}
      |]
  textGrid2 = T.unlines $ fmap textOneStep grids2

-- Resolution starts here

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
toList bounds grid = fmap (\row -> experiment (const row) grid) (allRows bounds)

countAllAccessible :: Bounds -> Grid Cell -> Int
countAllAccessible b = length . filter (== Just Accessible) . concat . toList b

renderGrid :: (Maybe a -> Char) -> Bounds -> Grid a -> String
renderGrid toChar bounds grid = unlines (fmap (fmap toChar) (toList bounds grid))

renderAccessibleCells :: Bounds -> Grid Cell -> String
renderAccessibleCells = renderGrid renderCell

solvePart1 :: Int -> [[Maybe Cell]] -> (Int, Bounds, Grid Cell)
solvePart1 threshold cells =
  let
    (bounds, initialGrid) = fromListUnsafe cells
    (count, grid, _) = step threshold bounds initialGrid
   in
    (count, bounds, grid)

pruneAccessible :: Grid Cell -> Grid Cell
pruneAccessible = fmap f
 where
  f Nothing = Nothing
  f (Just Accessible) = Nothing
  f (Just Inaccessible) = Just Inaccessible

step :: Int -> Bounds -> Grid Cell -> (Int, Grid Cell, Grid Cell)
step threshold bounds grid =
  let
    grid' = gridOfAccessibleSites threshold grid
    count = countAllAccessible bounds grid'
    pruned = pruneAccessible grid'

    -- IMPORTANT!!! force to create a fresh grid to avoid a tower of unevaluated grids
    cells = toList bounds pruned
    (_, gridFresh) = fromListUnsafe cells
   in
    (count, grid', gridFresh)

thermalisation :: Int -> Bounds -> Grid Cell -> [(Int, Grid Cell)]
thermalisation threshold bounds initialGrid = unfoldr evolve initialGrid
 where
  evolve :: Grid Cell -> Maybe ((Int, Grid Cell), Grid Cell)
  evolve previousGrid =
    let
      (count, gridWithAccessibleCells, newGrid) = step threshold bounds previousGrid
     in
      if count == 0 then Nothing else Just ((count, gridWithAccessibleCells), newGrid)

solvePart2 :: Int -> [[Maybe Cell]] -> (Int, Bounds, [(Int, Grid Cell)])
solvePart2 threshold cells =
  let
    (bounds, initialGrid) = fromListUnsafe cells
    evolution :: [(Int, Grid Cell)]
    evolution = thermalisation threshold bounds initialGrid
    count = sum $ fmap fst evolution
   in
    (count, bounds, evolution)

type Parser = Parsec Void T.Text
type ParsingError = ParseErrorBundle T.Text Void

cellParser :: Parser (Maybe Cell)
cellParser =
  char '@' $> Just Inaccessible
    <|> char '.' $> Nothing
    <|> char 'x' $> Nothing

rowParser :: Parser [Maybe Cell]
rowParser = some cellParser

gridParser :: Parser [[Maybe Cell]]
gridParser = sepEndBy rowParser newline

parse :: T.Text -> Either ParsingError [[Maybe Cell]]
parse = runParser gridParser "input day 4"
