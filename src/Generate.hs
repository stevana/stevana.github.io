{-# LANGUAGE OverloadedStrings #-}

module Generate where

import Data.Function (on)
import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import System.Directory
import System.FilePath
import Text.DocTemplates
import Text.Pandoc hiding (getDataFileName)

import Download (titleToFileName)
import Paths_stevana_github_io
import Types

------------------------------------------------------------------------

dATE_FORMAT :: String
dATE_FORMAT = "%h %e, %Y"

iNDEX_FILE :: FilePath
iNDEX_FILE = "index.html"

cHRONOLOGICAL_FILE :: FilePath
cHRONOLOGICAL_FILE = "chronological.html"

aBOUT_FILE :: FilePath
aBOUT_FILE = "about.html"

hTML5_TEMPLATE_FILE :: FilePath
hTML5_TEMPLATE_FILE = "html5.template"

cSS_FILE :: FilePath
cSS_FILE = "style.css"

rSS_SVG_FILE :: FilePath
rSS_SVG_FILE = "rss.svg"

------------------------------------------------------------------------

writeIndex :: Site -> FilePath -> IO ()
writeIndex site outputDir =
  T.writeFile (outputDir </> iNDEX_FILE) =<< generateReverseChronologicalIndex  site

-- writeReverseChronologicalIndex :: Site -> FilePath -> IO ()
-- writeReverseChronologicalIndex site outputDir =
--   T.writeFile (outputDir </> cHRONOLOGICAL_FILE) =<< generateReverseChronologicalIndex site

writeAbout :: Site -> FilePath -> IO ()
writeAbout site outputDir =
  T.writeFile (outputDir </> aBOUT_FILE) =<< generateAbout site

installCssAndSvg :: FilePath -> IO ()
installCssAndSvg outputDir = do
  cssFile <- getDataFileName ("data" </> cSS_FILE)
  copyFile cssFile (outputDir </> cSS_FILE)
  svgFeedFile <- getDataFileName ("data" </> rSS_SVG_FILE)
  copyFile svgFeedFile (outputDir </> rSS_SVG_FILE)

context :: [(Text, Text)] -> Context Text
context = toContext . Map.fromList

prepareTemplate :: IO (Template Text)
prepareTemplate = do
  templateFile <- getDataFileName ("data" </> hTML5_TEMPLATE_FILE)
  templateText <- runIOorExplode $ getTemplate templateFile
  eTemplate <- compileTemplate "" templateText
  case eTemplate of
    Left err       -> error $ "prepareTemplate: " ++ show err
    Right template -> return template

-- XXX: Maybe it's easier to do this on the pandoc AST instead?
extractTitle :: Text -> NonEmpty Text
extractTitle md
  | T.head md == '#' || T.head md == '-' = splitFirstLineApply True (T.dropWhile (/= '#') md)
  | otherwise = splitFirstLineApply False md
  where
    splitFirstLineApply b t = case T.lines t of
      l : l' : ls | b         -> T.drop 2 l :| l' : ls
                  | otherwise -> l :| ls
      _otherwise -> error "extractTitle: no title"

lookupDate :: FilePath -> Site -> Maybe UTCTime
lookupDate markdownFile
  = go
  . concatMap _posts
  . _topics
  where
    go [] = Nothing
    go (post : posts)
      | T.unpack (titleToFileName (_title post)) <.> "md" == takeFileName markdownFile = _date post
      | otherwise = go posts

markdownToHtml :: Site -> FilePath -> IO ()
markdownToHtml site markdownFile = do
  let mDate = lookupDate markdownFile site
  print (markdownFile, mDate)
  markdown <- T.readFile markdownFile
  let (title :| rest) = extractTitle markdown
      markdown' = T.unlines rest
  template <- prepareTemplate
  let readerOpts = def { readerExtensions = githubMarkdownExtensions <>
                                            pandocExtensions }
  pandoc <- runIOorExplode $ readMarkdown readerOpts markdown'
  let htmlFile = replaceExtensions markdownFile "html"
  html <- runIOorExplode $ do
    let writerOpts = def
          { writerTOCDepth = 4
          , writerTableOfContents = True
          , writerTemplate = Just template
          , writerVariables = context $
              [ ("lang", "en")
              , ("title", title)
              , ("path", T.pack (takeFileName htmlFile))
              , ("author-meta", _author site)
              , ("pagetitle", title)
              , ("css", T.pack cSS_FILE)
              , ("toc-title", "Table of contents")
              ] ++ (maybe []
                   (\date -> [("date", T.pack (formatTime defaultTimeLocale dATE_FORMAT date))]) mDate)

          }
    writeHtml5String writerOpts pandoc
  T.writeFile htmlFile html

------------------------------------------------------------------------

generatePage :: Site -> Text -> IO Text
generatePage site body = do
  template <- prepareTemplate
  runIOorExplode $ do
    pandoc <- readHtml def { readerExtensions = pandocExtensions } body
    let writerOpts = def
          { writerTemplate = Just template
          , writerVariables = context $
              [ ("lang", "en")
              , ("author-meta", _author site)
              , ("pagetitle", _name site)
              , ("css", T.pack cSS_FILE)
              ]
          }
    writeHtml5String writerOpts pandoc

-- generateIndex :: Site -> IO Text
-- generateIndex s = generatePage s
--   (ul (map (\t -> _topic t <> ul (concatMap displayPost (_posts t))) (_topics s)))

generateReverseChronologicalIndex :: Site -> IO Text
generateReverseChronologicalIndex s = generatePage s
  (ul (concatMap displayPost (sortBy (flip compare `on` _date) (concatMap _posts (_topics s)))))

displayPost :: Post -> [Text]
displayPost post =
  let
    link :: Text
    link = ahref (titleToFileName (_title post) <> ".html") (_title post) <>
           maybe "" displayDate (_date post)
  in
    case _status post of
      Done -> [link]
      FirstDraft -> ["Draft: " <> link]
      _otherwise -> []

displayDate :: UTCTime -> Text
displayDate d = mconcat
  [ "<div class=\"date\">posted on "
  , T.pack (formatTime defaultTimeLocale dATE_FORMAT d)
  , "</div>"
  ]

generateAbout :: Site -> IO Text
generateAbout s = generatePage s $ T.unlines $ map p $
  [ "How do we build reliable, scalable and maintainable computer systems?"

  , "This site contains notes on how, I think, we can improve on the state \
    \ of development, documentation, testing, deployment, observability, debugging, \
    \ and upgrading of distributed systems. Most of the ideas are stolen from \
    \ others, many from Erlang and Joe Armstrong. Over time I hope to turn this \
    \ into a more coherent text, for now think of it as a crude blog or some basic \
    \ scaffolding for me to hang my thoughts on."

  , "If you got comments, feedback or questions then feel free to get in \
    \ touch via <a href=\"mailto:4hsz4ji43@mozmail.com\">email</a>!"
  ]

------------------------------------------------------------------------

tag :: Text -> Text -> Text
tag x t = "<" <> x <> ">" <> t <> "</" <> x <> ">"

li :: Text -> Text
li = tag "li"

ul :: [Text] -> Text
ul ts = "<ul>\n" <> T.unlines (map (\t -> "  " <> li t) ts) <> "</ul>"

p :: Text -> Text
p = tag "p"

ahref :: Text -> Text -> Text
ahref link t = "<a href=\"" <> link <> "\">" <> t <> "</a>"
