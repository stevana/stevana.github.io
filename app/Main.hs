module Main where

import System.Environment
import System.Exit

import LibMain (libMain)

------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  let skipDownload = any ("--skip-download" ==) args
  case args of
    inFile : outputDir : _ -> libMain inFile outputDir skipDownload
    _otherwise -> do
      progName <- getProgName
      putStrLn ("Usage: " ++ progName ++ " IN-FILE OUT-DIR [--skip-download]")
      exitFailure
