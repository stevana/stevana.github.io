{-# LANGUAGE OverloadedStrings #-}

module Generate where

import Control.Monad (forM_)
import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import System.Directory
import System.FilePath
import Text.DocTemplates
import Text.Pandoc hiding (getDataFileName)
import Text.Pandoc.Walk (query, walk)

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

lINK_SVG_FILE :: FilePath
lINK_SVG_FILE = "link.svg"

jAVASCRIPT_FILE :: FilePath
jAVASCRIPT_FILE = "script.js"

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

installDataFiles :: FilePath -> IO ()
installDataFiles outputDir = do
  let files = [cSS_FILE, rSS_SVG_FILE, lINK_SVG_FILE, jAVASCRIPT_FILE]
  forM_ files $ \file -> do
    fp <- getDataFileName ("data" </> file)
    copyFile fp (outputDir </> file)

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

extractTitle :: Pandoc -> (Text, Pandoc)
extractTitle (Pandoc meta (Header 1 _attr inlines : blocks))
  = (extractText inlines, Pandoc meta blocks)
extractTitle _ = error "extractTitle: no title"

extractText :: [Inline] -> Text
extractText = T.concat . query aux
  where
    aux :: Inline -> [Text]
    aux (Str txt) = [txt]
    aux Space     = [" "]
    aux _         = []

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

-- Adds an anchor to all headings of level 2 or below, for more see:
-- https://danilafe.com/blog/blog_microfeatures/#easily-linkable-headings
addAnchorsToHeaders :: Pandoc -> Pandoc
addAnchorsToHeaders = walk aux
  where
    aux :: Block -> Block
    aux (Header lvl attr@(ident, _classes, _kvs) inlines)
      | lvl > 1 = Header lvl attr [Link nullAttr [Str title] ("#" <> ident, title)]
      where
        title :: Text
        title = extractText inlines
    aux b = b

markdownToHtml :: Site -> FilePath -> IO ()
markdownToHtml site markdownFile = do
  let mDate = lookupDate markdownFile site
  print (markdownFile, mDate)
  template <- prepareTemplate
  let readerOpts = def { readerExtensions = githubMarkdownExtensions <>
                                            pandocExtensions }
  -- anchorFilter <- getDataFileName ("data" </> aNCHOR_HEADINGS_LUA_FILE)
  markdown <- T.readFile markdownFile
  pandoc <- runIOorExplode $ readMarkdown readerOpts markdown
  let (title, pandoc') = extractTitle (addAnchorsToHeaders pandoc)
    -- applyFilter def anchorFilter pandoc
  let htmlFile = replaceExtensions markdownFile "html"
  html <- runIOorExplode $ do
    let writerOpts = def
          { writerTOCDepth = 4
          , writerTableOfContents = True
          , writerTemplate = Just template
          , writerSectionDivs = True
          , writerHTMLMathMethod  = MathJax defaultMathJaxURL
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
    writeHtml5String writerOpts pandoc'
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
  [ "How can we make it easier to build reliable, scalable and maintainable computer \
    \ systems?"

  , "This website is a collection of notes on how, I think, we can improve on the \
    \ state of development, documentation, testing, deployment, observability, \
    \ debugging, and upgrading of distributed systems."

  , "Many of the ideas I write about on these topics are inspired by the work of Joe \
    \ Armstrong (how to structure code to make it testable and reliable), Jim Gray \
    \ (implicit parallelism via pipelining) and Barbara Liskov (the importance of \
    \ programming language design to the spread of new ideas). Over time I hope to \
    \ turn these notes into a more coherent text, for now I like to think of it as \
    \ scaffolding for me to hang my thoughts on."

  , "If you got comments, feedback, questions or would like to work with me, then \
    \ feel free to get in touch via <a href=\"mailto:4hsz4ji43@mozmail.com\">email</a>."
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
