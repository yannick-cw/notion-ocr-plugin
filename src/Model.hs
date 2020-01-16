{-# LANGUAGE TemplateHaskell #-}

module Model  where 

import           Data.Text
import           Data.Time                      ( UTCTime )
import           Control.Lens                   ( makeLenses )

newtype NotionId = NotionId Text deriving (Eq)

data UserSyncState = Sync | NoSync
data User = User { _notionId :: NotionId,
                   _token :: Text,
                   _syncSetting :: UserSyncState,
                   _singleRunsInMonth :: Int,
                   _allowedRunsInMonth :: Int,
                   _imagesInMonth :: Int,
                   _allowedImagesInMonth :: Int,
                   _lastSync :: Maybe UTCTime }

makeLenses ''User
