module Repos.Notion
  ( NotionUser(..)
  , Notion(..)
  )
where

import           AppM
import           Data.Text                     as T
                                                ( Text )
import           Data.UUID                      ( toText )
import           Control.Monad.IO.Class         ( liftIO )
import           NotionApi.High
import           NotionApi.Types               as N
                                                ( UserData(..)
                                                , runNotionM
                                                )


newtype NotionUser = NotionUser { userId :: Text }

class Notion m where
  userInfo :: Text -> m NotionUser

instance Notion AppM where
  userInfo nId = do
    userData <- liftIO $ runNotionM nId getUserData
    let i = toText $ N.id userData
    return $ NotionUser i
