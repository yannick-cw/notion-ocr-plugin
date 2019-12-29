{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module AppM where

import           Control.Monad.Except
import           Data.Text
import           GHC.IO.Exception               ( ioe_description )

liftIOErr :: MonadError Text m => IOError -> m a
liftIOErr = throwError . pack . ioe_description

newtype AppM a = AppM { unwrap :: ExceptT Text  IO a }
  deriving (
              Functor
            , Applicative
            , Monad
            , MonadIO
            , MonadError Text
           )

