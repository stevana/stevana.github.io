module LibMain where

import Control.Monad
import Data.List
import System.Directory
import System.FilePath
import System.Process

import Download
import Generate
import Parse
import RssFeed

------------------------------------------------------------------------

libMain :: FilePath -> FilePath -> Bool -> IO ()
libMain inputFile outputDir skipDownload = do
  site <- parse inputFile
  createDirectoryIfMissing True outputDir
  markdowns <- downloadMarkdown skipDownload outputDir site
  cssModifiedAt <- readProcess "git" ["log", "-1", "--pretty=%at", "data/style.css"] ""
  let cssModifiedAt_ = dropWhileEnd (== '\n') cssModifiedAt
  print cssModifiedAt_
  writeIndex site cssModifiedAt outputDir
  -- writeReverseChronologicalIndex site outputDir
  writeAbout site cssModifiedAt outputDir
  mapM_ (markdownToHtml site cssModifiedAt_) markdowns
  installDataFiles outputDir
  installRssFeed site outputDir
  cleanUp outputDir skipDownload

cleanUp :: FilePath -> Bool -> IO ()
cleanUp outputDir skipDownload
  | skipDownload =  return ()
  | otherwise    = do
      files <- getDirectoryContents outputDir
      forM_ files $ \file ->
        when (takeExtension file == ".md") $
          removeFile (outputDir </> file)
