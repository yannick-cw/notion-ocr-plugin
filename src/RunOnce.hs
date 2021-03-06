{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module RunOnce
  ( runOnce
  )
where

import           Data.Text
import           InitState                      ( getOrCreateUser )
import           Repos.DB
import           Repos.Ocr
import           Repos.Notion
import           Model
import           Control.Lens
import           Control.Monad.Except           ( MonadError(..) )
import           Util.Utils                     ( guardM )

isEligible :: User -> Bool
isEligible user = user ^. singleRunsInMonth <= user ^. allowedRunsInMonth

runOnce :: (DB m, Notion m, MonadError Text m, Ocr m) => Text -> m ()
runOnce tkn = do
  user <- getOrCreateUser tkn
  guardM (isEligible user) "Can not run once"
  runOcr tkn
  addRunsUsed $ user ^. notionId
