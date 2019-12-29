{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module AppM where

import           Control.Monad.Except
import           Control.Monad.Reader           ( ReaderT
                                                , MonadReader
                                                )
import           Data.Text
import           GHC.IO.Exception               ( ioe_description )
import           Model
import           Control.Concurrent.STM.TVar    ( TVar )

liftIOErr :: MonadError Text m => IOError -> m a
liftIOErr = throwError . pack . ioe_description

newtype State = State { users :: TVar [User] }
newtype AppM a = AppM { unwrap :: ExceptT Text (ReaderT State IO) a }
  deriving (
              Functor
            , Applicative
            , Monad
            , MonadIO
            , MonadReader State
            , MonadError Text
           )

