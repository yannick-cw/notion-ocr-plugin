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
                                                )
import           Data.Text                      ( Text )
import           Data.Text.Lazy.Encoding        ( encodeUtf8 )
import           Data.Text.Lazy                 ( fromStrict )
import           RunOnce
import           SetSyncState

initState :: InitState
initState = InitState SyncOn

server :: (DB m, Notion m, Ocr m, MonadError Text m) => ServerT OcrApi m
server =
  maybe (fail "") getInitState
    :<|> maybe (fail "") runOnce
    :<|> maybe (fail "") setSyncState

ocrApi :: Proxy OcrApi
ocrApi = Proxy

nt :: AppM a -> Handler a
nt =
  Handler
    . withExceptT (\e -> err500 { errBody = encodeUtf8 $ fromStrict e })
    . unwrap


app :: Application
app = simpleCors $ serve ocrApi $ hoistServer ocrApi nt server


