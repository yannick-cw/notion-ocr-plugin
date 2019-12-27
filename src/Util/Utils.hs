{-# LANGUAGE FlexibleContexts #-}

module Util.Utils where

import           Control.Monad.Except           ( MonadError(..) )

guardM :: (MonadError e m) => Bool -> e -> m ()
guardM cond e = if cond then return () else throwError e
