{-# LANGUAGE OverloadedStrings #-}

module Download where

import Control.Monad
import qualified Data.ByteString.Lazy as LBS
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (ok200)
import System.FilePath

import Types

------------------------------------------------------------------------

downloadMarkdown :: Bool -> FilePath -> Site -> IO [(FilePath, Maybe UTCTime)]
downloadMarkdown skip dir
  = fmap concat
  . mapM go -- XXX: Can be done concurrenctly.
  . concatMap (map (\post -> (_title post, _content post, _date post)))
  . map _posts
  . _topics
  where
    go :: (Text, Maybe Text, Maybe UTCTime) -> IO [(FilePath, Maybe UTCTime)]
    go (_title, Nothing, _mDate) = return []
    go (title, Just url, mDate) = do
      let fp = dir </> T.unpack (titleToFileName title) <.> "md"
      if skip then return [(fp, mDate)]
      else do
        mgr <- newManager tlsManagerSettings
        req <- parseRequest (T.unpack url)
        resp <- httpLbs req mgr
        when (responseStatus resp /= ok200) $
          error ("downloadMarkdown: failed to download: " ++ T.unpack url)
        LBS.writeFile fp (responseBody resp)
        return [(fp, mDate)]

titleToFileName :: Text -> Text
titleToFileName
  = T.toLower
  . T.replace " " "_"
  . T.replace "à" "a"
  . T.replace "á" "a"
  . T.filter (\c -> isAlphaNum c || c == ' ' || c == '-')


-- git clone --filter=tree:0 https://github.com/stevana/pipelining-with-disruptor
-- git log -1 --pretty='format:%aI' README.md
-- 2023-10-16T13:47:10+02:00
-- https://github.com/stevana/pipelining-with-disruptor/commits/main/README.md
