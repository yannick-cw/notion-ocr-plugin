module ScheduledRun where

import           Repos.Ocr
import           Repos.DB
import           Model
import           Data.Foldable                  ( traverse_ )
import           Control.Lens

isEligible :: User -> Bool
isEligible user = user ^. imagesInMonth <= user ^. allowedImagesInMonth

updateAll :: (DB m, Ocr m, Monad m) => m ()
updateAll = do
  users <- getAllUsers
  let eligiableUsers = filter isEligible users
  traverse_ runOcr (eligiableUsers ^.. each . token)
