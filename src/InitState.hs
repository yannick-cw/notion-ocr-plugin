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


getInitState :: (DB m, Notion m, Monad m) => Text -> m InitState
getInitState =
  fmap
      (\user -> InitState
        { syncState = case syncSetting user of
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
  newUser nId = User { notionId             = NotionId nId
                     , token                = tkn
                     , syncSetting          = NoSync
                     , singleRunsInMonth    = 0
                     , allowedRunsInMonth   = 100
                     , imagesInMonth        = 0
                     , allowedImagesInMonth = 100
                     , lastSync             = Nothing
                     }

