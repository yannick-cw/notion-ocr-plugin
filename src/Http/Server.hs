module Http.Server where

import           Http.Api
import           Servant

initState = Just (InitState SyncOn)

server :: Server OcrApi
server _ = return initState

ocrApi :: Proxy OcrApi
ocrApi = Proxy

app :: Application
app = serve ocrApi server
