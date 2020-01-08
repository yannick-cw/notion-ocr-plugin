{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Repos.Notion
  ( NotionUser(..)
  , Notion(..)
  )
where

import           AppM
import           Data.Text                     as T
                                                ( Text
                                                , unpack
                                                )
import           Data.UUID                      ( UUID
                                                , fromString
                                                , toText
                                                )
import           Data.HashMap.Strict            ( keys )
import           Control.Lens
import           Network.Wreq
import           Data.Aeson.Types               ( emptyObject )
import           Data.Aeson.Lens                ( key
                                                , _Object
                                                )
import           Data.Maybe                     ( maybeToList )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.ByteString.Char8         as C8
                                                ( pack )
import           Network.HTTP.Client            ( Cookie(..)
                                                , createCookieJar
                                                , CookieJar
                                                )
import           Data.Time.Clock


notionUrl :: String
notionUrl = "https://www.notion.so/api/v3/"

newtype NotionUser = NotionUser { userId :: Text }

class Notion m where
  userInfo :: Text -> m NotionUser

instance Notion AppM where
  userInfo = (fmap (NotionUser . toText . Prelude.head)) . loadUserSpaces

data NotionSearchRes = NotionSearchRes { insertId :: UUID ,  imageURL :: Text } deriving ( Show)

loadUserSpaces :: Text -> AppM [UUID]
loadUserSpaces tkn = do
  opts <- AppM $ liftIO (cookieOpts tkn)
  let url = notionUrl ++ "loadUserContent"
  r <- liftIO $ postWith opts url emptyObject
  return
    $   maybeToList
          (r ^? responseBody . key "recordMap" . key "notion_user" . _Object)
    >>= keys
    >>= maybeToList
    .   fromString
    .   T.unpack

notionCookie :: Text -> IO CookieJar
notionCookie token = do
  now <- liftIO getCurrentTime
  let expires = addUTCTime (1440 * 3000) now
      c       = Cookie "token_v2"
                       (C8.pack $ T.unpack token)
                       expires
                       "notion.so"
                       "/"
                       now
                       now
                       False
                       False
                       True
                       True
  return $ createCookieJar [c]

cookieOpts :: Text -> IO Options
cookieOpts nId = (\jar -> defaults & cookies ?~ jar) <$> (notionCookie nId)
