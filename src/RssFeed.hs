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
      html <- T.readFile (outputDir </> T.unpack topic <.> "html")
      let main
            = T.unlines
            . takeWhile (/= "</main>")
            . drop 1
            . dropWhile (/= "<main>")
            . T.lines
            $ html
      return (Map.insert topic main ih)

------------------------------------------------------------------------

rssFeed :: Site -> Map Text Text -> Text
rssFeed s descs = T.unlines
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  , "<rss version=\"2.0\" xmlns:atom=\"http://www.w3.org/2005/Atom\">"
  , "  <channel>"
  , "    <title>" <> _name s <> "</title>"
  , "    <description>" <> _synopsis s <> "</description>"
  , "    <language>en</language>"
  , "    <link>" <> _url s <> "/rss.xml" <> "</link>"
  , "    <atom:link href=\"" <> _url s <> "/rss.xml\" rel=\"self\" type=\"application/rss+xml\" />"
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
  , "      <link>" <> link <> "</link>"
  , "      <guid>" <> link <> "</guid>"
  , "      <pubDate>" <> date <> "</pubDate>"
  , "      <description><![CDATA[" <> escapeCDataEndTag (descs Map.! titleToFileName (_title p)) <> "]]></description>"
  , "      <category>" <> topic <> "</category>"
  , "    </item>"
  ]
  where
    link = url <> "/" <> titleToFileName (_title p) <> ".html"
    date
      = T.replace "  " " " -- Remove excessive spaces.
      . T.replace "UTC" "GMT" -- For some reason rfc822DateFormat outputs "UTC" which isn't valid.
      . T.pack . formatTime defaultTimeLocale rfc822DateFormat
      . fromMaybe (error "item: expected a date")
      $ _date p

    -- Since there's an end of CDATA tag ("]]>") in one of my code examples, we
    -- need to escape it.
    escapeCDataEndTag = T.replace "]]>" "]]]]><![CDATA[>"
