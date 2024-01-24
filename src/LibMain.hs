module LibMain where

import Control.Monad
import System.Directory
import System.FilePath

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
  writeIndex site outputDir
  -- writeReverseChronologicalIndex site outputDir
  writeAbout site outputDir
  mapM_ (markdownToHtml site) markdowns
  installCssAndSvg outputDir
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
