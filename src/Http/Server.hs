{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Http.Server where

import           Http.Api
import           Servant
import           Network.Wai.Middleware.Cors    ( simpleCors )
import           AppM
import           InitState
import           Repos.DB
import           Repos.Notion
import           Repos.Ocr
import           Control.Monad.Except           ( withExceptT
                                                , MonadError
                                                , mapExceptT
                                                )
import           Control.Monad.Reader           ( runReaderT )
import           Data.Text                      ( Text )
import           Data.Text.Lazy.Encoding        ( encodeUtf8 )
import           Data.Text.Lazy                 ( fromStrict )
import           RunOnce
import           SetSyncState

initState :: InitState
initState = InitState SyncOn

server :: (DB m, Notion m, Ocr m, MonadError Text m) => ServerT OcrApi m
server =
  maybe (fail missingTkn) getInitState
    :<|> maybe (fail missingTkn) runOnce
    :<|> maybe (fail missingTkn) setSyncState
  where missingTkn = "Please specify the token parameter"

ocrApi :: Proxy OcrApi
ocrApi = Proxy

nt :: State -> AppM a -> Handler a
nt s =
  Handler
    . mapExceptT (`runReaderT` s)
    . withExceptT (\e -> err500 { errBody = encodeUtf8 $ fromStrict e })
    . unwrap


app :: State -> Application
app s = simpleCors $ serve ocrApi $ hoistServer ocrApi (nt s) server


