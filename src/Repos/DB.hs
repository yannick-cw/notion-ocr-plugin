module Repos.DB
  ( DB(..)
  , User(..)
  , UserSyncState(..)
  , NotionId(..)
  )
where

import           Data.Text                      ( Text )
import           AppM
import           Model
import           Control.Monad.Reader           ( ask )
import           Control.Concurrent.STM.TVar    ( readTVarIO
                                                , writeTVar
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.List                      ( find )
import           GHC.Conc                       ( atomically )

class DB m where
  findInitState :: Text -> m (Maybe User)
  insertUser :: User -> m ()
  addRunsUsed :: NotionId -> m ()
  addImagesUsed :: NotionId -> m ()
  setUserSyncState :: NotionId -> UserSyncState -> m ()
  getAllUsers :: m [User]

instance DB AppM where
  findInitState tkn = do
    State { users = us } <- ask
    users'               <- liftIO $ readTVarIO us
    return (find ((== tkn) . token) users')
  insertUser user = do
    State { users = us } <- ask
    users'               <- liftIO $ readTVarIO us
    liftIO $ atomically $ writeTVar us (user : users')
  addRunsUsed _ = undefined--do
    --State { users = us } <- ask
    --users'               <- liftIO $ readTVarIO us
    --let user = (find ((== nId) . notionId) users')
    --let updatedUser =
          --(\u -> u { singleRunsInMonth = singleRunsInMonth u + 1 }) <$> user
    --return $ maybe (return ()) (atomically $ writeTVar )
  addImagesUsed    = undefined
  setUserSyncState = undefined
  getAllUsers      = undefined
