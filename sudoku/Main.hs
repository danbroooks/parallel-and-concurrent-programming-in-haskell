module Main where

import           Control.Monad
import           Control.Parallel.Strategies hiding (parMap)
import qualified Data.Maybe as M
import           Data.Monoid ((<>))
import           Sudoku
import           System.Environment

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
        (Nothing, _) -> putStrLn "Please provide valid execution mode: 'par' / 'seq'"
        _ -> putStrLn "Please choose a file, 1000 / 16000 / 49151"

    getExecMode =
      (parseExecMode <=< M.listToMaybe) <$> getArgs

    getFilepath =
      (parseFilepath <=< M.listToMaybe . drop 1) <$> getArgs

    parseExecMode "par" = Just ExecPar
    parseExecMode "seq" = Just ExecSeq
    parseExecMode _ = Nothing

    parseFilepath str =
      (\n -> "sudoku17." <> n <> ".txt") <$> case str of
        "1000" -> Just "1000"
        "16000" -> Just "16000"
        "49151" -> Just "49151"
        _ -> Nothing

sudoku :: ExecutionMode -> String -> IO ()
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
      print . length . filter M.isJust

    readPuzzles =
      lines <$> readFile ("sudoku/data/" <> filepath)

parMap :: (a -> b) -> [a] -> Eval [b]
parMap _ [] = return []
parMap f (a:as) = do
   b <- rpar (f a)
   bs <- parMap f as
   return (b:bs)
