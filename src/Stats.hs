module Stats where

import Args (StatsRender (..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Exit (exitFailure)

program :: Int -> StatsRender -> IO ()
program year render = do
  T.putStrLn $
    case render of
      ConsoleRender -> "Stats are temporarily disabled for the GHC 9.14.1 compatibility check."
      JsonRender -> "{\"error\":\"Stats are temporarily disabled for the GHC 9.14.1 compatibility check.\"}"
  T.putStrLn ("Requested year: " <> T.pack (show year))
  exitFailure
