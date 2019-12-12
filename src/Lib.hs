module Lib
  ( someFunc
  )
where

import           Http.Server
import           Network.Wai.Handler.Warp       ( run )

someFunc :: IO ()
someFunc = run 8081 app
