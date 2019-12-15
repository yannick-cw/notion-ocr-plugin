module Http.Server where

import           Http.Api
import           Servant
import           Network.Wai.Middleware.Cors    ( simpleCors )

initState = Just (InitState SyncOn)

server :: Server OcrApi
server =
  (\_ -> return initState) :<|> (\_ -> return ()) :<|> (\_ _ -> return ())

ocrApi :: Proxy OcrApi
ocrApi = Proxy

app :: Application
app = simpleCors $ serve ocrApi server
