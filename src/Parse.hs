module Parse where

import Data.Yaml

import Types

------------------------------------------------------------------------

parse :: FilePath -> IO Site
parse = decodeFileThrow
