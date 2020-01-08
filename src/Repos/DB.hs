{-# LANGUAGE TupleSections #-}

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
                                                , TVar
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
    (users', _) <- readUsrs
    return (find ((== tkn) . token) users')

  insertUser user = do
    (users', us) <- readUsrs
    liftIO $ atomically $ writeTVar us (user : users')

  addRunsUsed nId = do
    (users', us) <- readUsrs
    let updateSync u = u { singleRunsInMonth = singleRunsInMonth u + 1 }
    let updatedUsers = updateFieldOnId nId updateSync users'
    liftIO $ atomically $ writeTVar us updatedUsers

  addImagesUsed nId = do
    (users', us) <- readUsrs
    let updateSync u = u { imagesInMonth = imagesInMonth u + 1 }
    let updatedUsers = updateFieldOnId nId updateSync users'
    liftIO $ atomically $ writeTVar us updatedUsers

  setUserSyncState nId syncState = do
    (users', us) <- readUsrs
    let updateSync u = u { syncSetting = syncState }
    let updatedUsers = updateFieldOnId nId updateSync users'
    liftIO $ atomically $ writeTVar us updatedUsers

  getAllUsers = fst <$> readUsrs

readUsrs :: AppM ([User], TVar [User])
readUsrs = do
  State { users = us } <- ask
  liftIO $ (, us) <$> readTVarIO us

updateFieldOnId :: NotionId -> (User -> User) -> [User] -> [User]
updateFieldOnId nId f users' =
  (\user -> if notionId user == nId then f user else user) <$> users'
