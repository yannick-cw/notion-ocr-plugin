{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module InitState
  ( getInitState
  , NotionUser(..)
  )
where

import           Http.Api
import           Data.Text
import           Repos.DB
import           Repos.Notion


getInitState :: (DB m, Notion m, Monad m) => Text -> m InitState
getInitState tkn = do
  maybeUser <- findInitState tkn
  user      <- maybe (createUser tkn) return maybeUser
  let mapSyncState = case syncSetting user of
        Sync   -> SyncOn
        NoSync -> SyncOff
  return $ InitState { syncState = mapSyncState }

createUser :: (Monad m, Notion m, DB m) => Text -> m User
createUser tkn = do
  notionUser <- userInfo tkn
  let user = newUser $ userId notionUser
  insertUser user
  return user
 where
  newUser nId = User { notionId             = nId
                     , token                = tkn
                     , syncSetting          = NoSync
                     , singleRunsInMonth    = 0
                     , allowedRunsInMonth   = 100
                     , imagesInMonth        = 0
                     , allowedImagesInMonth = 100
                     , lastSync             = Nothing
                     }

