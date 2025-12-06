module App where

import Args (
  Args (Args),
  Command (..),
  CommandWithGlobalOptions (CommandWithGlobalOptions),
  GenerateArgs (GenerateArgs),
  StatsArgs (StatsArgs),
  Verbosity,
  parseArgs,
  readInput,
 )
import CodeGenerator (program)
import qualified Data.Text as T
import Options.Applicative (handleParseResult)
import Solutions (solutions)
import Stats (program)
import System.Environment (getArgs)

program :: IO ()
program =
  getArgs >>= (handleParseResult . parseArgs) >>= unwrapGlobalOptions program'

program' :: Verbosity -> Command -> IO ()
program' verbosity (Run (Args day input)) = readInput input >>= runSolution verbosity day
program' _ (Generate (GenerateArgs d)) = CodeGenerator.program d
program' _ (GetStats (StatsArgs year export)) = Stats.program year export

runSolution :: Verbosity -> Int -> T.Text -> IO ()
runSolution verbosity day input = case solution day of
  Just solve -> solve verbosity input
  Nothing -> putStrLn "day not found"

solution :: Int -> Maybe (Verbosity -> T.Text -> IO ())
solution day
  | day >= 1 && day <= length solutions = Just (solutions !! (day - 1))
  | otherwise = Nothing

unwrapGlobalOptions :: (Verbosity -> Command -> a) -> CommandWithGlobalOptions -> a
unwrapGlobalOptions f (CommandWithGlobalOptions v c) = f v c
