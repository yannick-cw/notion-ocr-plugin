{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module InitState
  ( getInitState
  , getOrCreateUser
  , NotionUser(..)
  )
where

import           Http.Api
import           Data.Text
import           Repos.DB
import           Repos.Notion
import           Control.Lens
import           Model


getInitState :: (DB m, Notion m, Monad m) => Text -> m InitState
getInitState =
  fmap
      (\user -> InitState
        { syncState = case user ^. syncSetting of
                        Sync   -> SyncOn
                        NoSync -> SyncOff
        }
      )
    . getOrCreateUser

getOrCreateUser :: (DB m, Notion m, Monad m) => Text -> m User
getOrCreateUser tkn = findInitState tkn >>= maybe (createUser tkn) return

createUser :: (Monad m, Notion m, DB m) => Text -> m User
createUser tkn = do
  notionUser <- userInfo tkn
  let user = newUser $ userId notionUser
  insertUser user
  return user
 where
  newUser nId = User { _notionId             = NotionId nId
                     , _token                = tkn
                     , _syncSetting          = NoSync
                     , _singleRunsInMonth    = 0
                     , _allowedRunsInMonth   = 100
                     , _imagesInMonth        = 0
                     , _allowedImagesInMonth = 100
                     , _lastSync             = Nothing
                     }

