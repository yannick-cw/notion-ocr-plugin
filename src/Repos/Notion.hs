module Repos.Notion
  ( NotionUser(..)
  , Notion(..)
  )
where

import           Data.Text
import           AppM

newtype NotionUser = NotionUser { userId :: Text }

class Notion m where
  userInfo :: Text -> m NotionUser

instance Notion AppM where
  userInfo = undefined
