module Repos.DB
  ( DB(..)
  , User(..)
  , UserSyncState(..)
  , NotionId(..)
  )
where

import           Data.Text
import           Data.Time                      ( UTCTime )
import           AppM

newtype NotionId = NotionId Text

data UserSyncState = Sync | NoSync
data User = User { notionId :: NotionId,
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
  addRunsUsed :: NotionId -> m ()
  addImagesUsed :: NotionId -> m ()
  setUserSyncState :: NotionId -> UserSyncState -> m ()
  getAllUsers :: m [User]

instance DB AppM where
  findInitState    = undefined
  insertUser       = undefined
  addRunsUsed      = undefined
  addImagesUsed    = undefined
  setUserSyncState = undefined
  getAllUsers      = undefined
