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
                                                , modifyTVar
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.List                      ( find )
import           GHC.Conc                       ( atomically )
import           Control.Lens

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
    return (find ((== tkn) . view token) users')

  insertUser user = updateUsers (user :)

  addRunsUsed nId =
    updateUsers $ updateFieldOnId nId (singleRunsInMonth %~ (+ 1))

  addImagesUsed nId =
    updateUsers $ updateFieldOnId nId (imagesInMonth %~ (+ 1))

  setUserSyncState nId syncState =
    updateUsers $ updateFieldOnId nId (syncSetting .~ syncState)

  getAllUsers = do
    State { users = us } <- ask
    liftIO $ readTVarIO us

updateUsers :: ([User] -> [User]) -> AppM ()
updateUsers f = do
  State { users = us } <- ask
  liftIO $ atomically $ modifyTVar us f

updateFieldOnId :: NotionId -> (User -> User) -> [User] -> [User]
updateFieldOnId nId f users' =
  (\user -> if user ^. notionId == nId then f user else user) <$> users'
