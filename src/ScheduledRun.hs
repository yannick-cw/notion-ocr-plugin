module ScheduledRun where

import           Repos.Ocr
import           Repos.DB
import           Data.Foldable                  ( traverse_ )

isEligible :: User -> Bool
isEligible user = imagesInMonth user <= allowedImagesInMonth user

updateAll :: (DB m, Ocr m, Monad m) => m ()
updateAll = do
  users <- getAllUsers
  let eligiableUsers = filter isEligible users
  traverse_ runOcr (token <$> eligiableUsers)
