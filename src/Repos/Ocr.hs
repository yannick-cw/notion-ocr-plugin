module Repos.Ocr where

import           AppM
import           Data.Text

class Ocr m where
  runOcr :: Text -> m ()

instance Ocr AppM where
  runOcr = undefined
