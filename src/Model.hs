module Model where

import           Data.Text
import           Data.Time                      ( UTCTime )

newtype NotionId = NotionId Text deriving (Eq)

data UserSyncState = Sync | NoSync
data User = User { notionId :: NotionId,
                   token :: Text,
                   syncSetting :: UserSyncState,
                   singleRunsInMonth :: Int,
                   allowedRunsInMonth :: Int,
                   imagesInMonth :: Int,
                   allowedImagesInMonth :: Int,
                   lastSync :: Maybe UTCTime }
