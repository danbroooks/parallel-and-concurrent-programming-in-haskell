module Main where

import           Control.Parallel.Strategies hiding (parMap)
import qualified Data.Text as T
import           Prelude
import qualified Prelude.List as L
import           Sudoku

data ExecutionMode
  = ExecPar
  | ExecSeq

main :: IO ()
main = do
  execMode <- getExecMode
  filepath <- getFilepath
  attemptRun execMode filepath
  where
    attemptRun execMode filepath =
      case (execMode, filepath) of
        (Just mode, Just file) -> sudoku mode file
        (Nothing, _) -> putText "Please provide valid execution mode: 'par' / 'seq'"
        _ -> putText "Please choose a file, 1000 / 16000 / 49151"

    getExecMode =
      (parseExecMode <=< L.head) <$> getArgs

    getFilepath =
      (parseFilepath <=< L.head . drop 1) <$> getArgs

    parseExecMode "par" = Just ExecPar
    parseExecMode "seq" = Just ExecSeq
    parseExecMode _ = Nothing

    parseFilepath str =
      (\n -> "sudoku17." <> n <> ".txt") <$> case str of
        "1000" -> Just "1000"
        "16000" -> Just "16000"
        "49151" -> Just "49151"
        _ -> Nothing

sudoku :: ExecutionMode -> Text -> IO ()
sudoku execMode filepath = readPuzzles >>= displayResults . determineMode
  where
    determineMode =
      case execMode of
        ExecPar -> runParallel
        ExecSeq -> runSequential

    runParallel =
      runEval . parMap solve

    runSequential=
      fmap solve

    displayResults =
      print . length . filter isJust

    readPuzzles =
      T.lines <$> (readFile . T.unpack $ "sudoku/data/" <> filepath)

parMap :: (a -> b) -> [a] -> Eval [b]
parMap _ [] = return []
parMap f (a:as) = do
   b <- rpar (f a)
   bs <- parMap f as
   return (b:bs)
