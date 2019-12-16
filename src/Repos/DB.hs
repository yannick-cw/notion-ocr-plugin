module Repos.DB
  ( DB(..)
  , User(..)
  , UserSyncState(..)
  )
where

import           Data.Text
import           Data.Time                      ( UTCTime )
import           AppM

data UserSyncState = Sync | NoSync
data User = User { notionId :: Text,
                   token :: Text,
                   syncSetting :: UserSyncState,
                   singleRunsInMonth :: Int,
                   allowedRunsInMonth :: Int,
                   imagesInMonth :: Int,
                   allowedImagesInMonth :: Int,
                   lastSync :: Maybe UTCTime }


class DB m where
  findInitState :: Text -> m (Maybe User)
  insertUser :: User -> m ()

instance DB AppM where
  findInitState = undefined
  insertUser    = undefined
