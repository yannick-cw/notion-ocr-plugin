
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module SetSyncState
  ( setSyncState
  )
where

import           Data.Text
import           InitState                      ( getOrCreateUser )
import           Repos.DB
import           Repos.Notion
import           Http.Api                       ( SyncState(..) )

setSyncState :: (DB m, Notion m, Monad m) => Text -> SyncState -> m ()
setSyncState tkn syncState = do
  user <- getOrCreateUser tkn
  setUserSyncState
    (notionId user)
    (case syncState of
      SyncOn  -> Sync
      SyncOff -> NoSync
    )
