module LibMain where

import Control.Monad
import System.Directory
import System.FilePath

import Download
import Generate
import Parse
import RssFeed
import Types

------------------------------------------------------------------------

libMain :: FilePath -> FilePath -> Bool -> IO ()
libMain inputFile outputDir skipDownload = do
  site <- parse inputFile
  createDirectoryIfMissing True outputDir
  markdowns <- downloadMarkdown skipDownload outputDir site
  writeIndex site outputDir
  writeReverseChronologicalIndex site outputDir
  writeAbout site outputDir
  mapM_ (markdownToHtml (_name site) (_author site)) markdowns
  installCssAndSvg outputDir
  installRssFeed site outputDir
  cleanUp outputDir

cleanUp :: FilePath -> IO ()
cleanUp outputDir = do
  files <- getDirectoryContents outputDir
  forM_ files $ \file ->
    when (takeExtension file == ".md" || takeExtension file == ".txt") $
      removeFile (outputDir </> file)
