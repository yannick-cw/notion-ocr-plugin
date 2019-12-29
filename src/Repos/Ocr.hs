{-# LANGUAGE PackageImports #-}

module Repos.Ocr where

import           AppM
import qualified "notion-ocr" AppM             as OcrAppM
                                                ( AppM(unwrap) )
import           Data.Text
import           Updater                        ( updateOcrs )
import           Control.Monad.Except           ( mapExceptT )
import           Control.Monad.Reader           ( withReaderT )
import           CliParser
import           System.IO.Temp                 ( withSystemTempDirectory )


class Ocr m where
  runOcr ::  Text -> m ()

instance Ocr AppM where
  runOcr tkn = AppM
    $ withSystemTempDirectory "notion_ocr_plug" transformOcrAppToAppM
   where
    transformOcrAppToAppM tmpFile = mapExceptT
      (withReaderT $ const
        (Args { tempPath    = tmpFile
              , notionToken = tkn
              , schedule    = Nothing
              , verbose     = True
              }
        )
      )
      (OcrAppM.unwrap updateOcrs)

