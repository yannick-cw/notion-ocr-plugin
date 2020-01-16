
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
import           Control.Lens
import           Model

setSyncState :: (DB m, Notion m, Monad m) => Text -> SyncState -> m ()
setSyncState tkn syncState = do
  user <- getOrCreateUser tkn
  setUserSyncState
    (user ^. notionId)
    (case syncState of
      SyncOn  -> Sync
      SyncOff -> NoSync
    )
