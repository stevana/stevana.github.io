{-# LANGUAGE OverloadedStrings #-}

module RssFeed where

import Control.Monad
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import System.FilePath
import Data.Map (Map)
import qualified Data.Map as Map

import Download (titleToFileName)
import Types

------------------------------------------------------------------------

rSS_FILE :: String
rSS_FILE = "rss.xml"

------------------------------------------------------------------------

installRssFeed :: Site -> FilePath -> IO ()
installRssFeed s outputDir = do
  descs <- readDescriptions s outputDir
  T.writeFile (outputDir </> rSS_FILE) (rssFeed s descs)

readDescriptions :: Site -> FilePath -> IO (Map Text Text)
readDescriptions s outputDir = do
  let topics
        = map (titleToFileName . _title)
        . filter donePosts
        . concatMap _posts
        . _topics
        $ s
  foldM go Map.empty topics
  where
    go ih topic = do
      txt <- T.readFile (outputDir </> T.unpack topic <.> "txt")
      return (Map.insert topic txt ih)

------------------------------------------------------------------------

rssFeed :: Site -> Map Text Text -> Text
rssFeed s descs = T.unlines
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  , "<rss version=\"2.0\">"
  , "  <channel>"
  , "    <title>" <> _name s <> "</title>"
  , "    <description>" <> _synopsis s <> "</description>"
  , "    <language>en</language>"
  , "    <link>" <> _url s <> "</link>"
  ,      items s descs
  , "  </channel>"
  , "</rss>"
  ]

items :: Site -> Map Text Text -> Text
items s descs
  = T.unlines
  . map (item (_url s) descs)
  . filter (donePosts . snd)
  . sortBy (flip compare `on` (_date . snd))
  . concatMap (\topic -> cycle [_topic topic] `zip` (_posts topic))
  . _topics
  $ s

donePosts :: Post -> Bool
donePosts p = _status p == Done

item :: Text -> Map Text Text -> (Text, Post) -> Text
item url descs (topic, p) = T.unlines
  [ "    <item>"
  , "      <title>" <> _title p <> "</title>"
  , "      <link>" <> url <> "/" <> titleToFileName (_title p) <> ".html" <> "</link>"
  , "      <pubDate>" <> date <> "</pubDate>"
  , "      <description><![CDATA[" <> escapeCDataEndTag (descs Map.! titleToFileName (_title p)) <> "]]></description>"
  , "      <category>" <> topic <> "</category>"
  , "    </item>"
  ]
  where
    date = T.pack (formatTime defaultTimeLocale rfc822DateFormat (fromMaybe (error "item: expected a date") (_date p)))

    -- Since there's an end of CDATA tag ("]]>") in one of my code examples, we
    -- need to escape it.
    escapeCDataEndTag = T.replace "]]>" "]]]]><![CDATA[>"
