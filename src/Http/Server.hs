{-# LANGUAGE OverloadedStrings #-}

module Http.Server where

import           Http.Api
import           Servant
import           Network.Wai.Middleware.Cors    ( simpleCors )
import           AppM
import           InitState
import           Repos.DB
import           Repos.Notion
import           Control.Monad.Except           ( withExceptT )
import           Data.Text.Lazy.Encoding        ( encodeUtf8 )
import           Data.Text.Lazy                 ( fromStrict )

initState :: InitState
initState = InitState SyncOn

server :: (Monad m, DB m, Notion m) => ServerT OcrApi m
server =
  (maybe (fail "") getInitState) :<|> (\_ -> return ()) :<|> (\_ _ -> return ())

ocrApi :: Proxy OcrApi
ocrApi = Proxy

nt :: AppM a -> Handler a
nt x = Handler
  (withExceptT (\e -> err500 { errBody = encodeUtf8 $ fromStrict e }) (unwrap x)
  )

app :: Application
app = simpleCors $ serve ocrApi $ hoistServer ocrApi nt server


