{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

module Types where

import Data.Time
import Data.Text (Text)
import Data.Yaml
import Data.Aeson
import GHC.Generics

------------------------------------------------------------------------

data Site = Site
  { _name     :: Text
  , _author   :: Text
  , _synopsis :: Text
  , _url      :: Text
  , _topics   :: [Topic]
  }
  deriving stock (Generic, Show)

data Topic = Topic
  { _topic :: Text
  , _posts :: [Post]
  }
  deriving stock (Generic, Show)

data Post = Post
  { _title   :: Text
  , _content :: Maybe Text
  , _date    :: Maybe UTCTime
  , _status  :: Status
  }
  deriving stock (Generic, Show)


data Status = Done | Planned | Research | FirstDraft
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON)

------------------------------------------------------------------------

customOptions :: Options
customOptions = defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON Site where
  parseJSON = genericParseJSON customOptions

instance FromJSON Topic where
  parseJSON = genericParseJSON customOptions

instance FromJSON Post where
  parseJSON = genericParseJSON customOptions
