module Main where

import           Control.DeepSeq
import           Control.Monad
import           Control.Parallel.Strategies
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
sudoku execMode filepath = determineMode
  where
    determineMode =
      case execMode of
        ExecPar -> runParallel
        ExecSeq -> runSequential

    runParallel = do
      (as, bs) <- (\p -> splitAt (length p `div` 2) p) . lines <$> readFile ("sudoku/data/" <> filepath)
      print . length . filter M.isJust . runEval $ do
        as' <- rpar (force (map solve as))
        bs' <- rpar (force (map solve bs))
        _ <- rseq as'
        _ <- rseq bs'
        return (as' ++ bs')
      return ()

    runSequential = do
      puzzles <- lines <$> readFile ("sudoku/data/" <> filepath)
      print . length . filter M.isJust $ solve <$> puzzles
      return ()
