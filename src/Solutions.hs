module Solutions where

import qualified Data.Text as T
import Day1 (program)
import Day10 (program)
import Day2 (program)
import Day3 (program)
import Day4 (program)
import Day5 (program)
import Day6 (program)
import Day7 (program)
import Day8 (program)
import Day9 (program)
import Args (Verbosity)

solutions :: [Verbosity -> T.Text -> IO ()]
solutions =
  [ const $ Day1.program
  , const $ Day2.program
  , const $ Day3.program
  , Day4.program
  , const $ Day5.program
  , const $ Day6.program
  , const $ Day7.program
  , const $ Day8.program
  , const $ Day9.program
  , const $ Day10.program
  ]
