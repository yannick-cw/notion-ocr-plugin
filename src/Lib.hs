module Lib
  ( someFunc
  )
where

import           Http.Server
import           Network.Wai.Handler.Warp       ( run )
import           Control.Concurrent             ( forkIO
                                                , killThread
                                                , threadDelay
                                                )
import           Control.Exception              ( bracket )
import           Control.Monad.Except           ( runExceptT )
import           ScheduledRun
import           AppM                           ( AppM(..) )
import qualified Data.Text.IO                  as TIO
                                                ( putStrLn )

someFunc :: IO ()
someFunc = bracket (forkIO $ run 8081 app) killThread $ \_ -> runScheduled
  (either TIO.putStrLn (const $ return ()) =<< runExceptT (unwrap updateAll))
  30

runScheduled :: IO () -> Int -> IO ()
runScheduled job pauseTimeMinutes = do
  putStrLn
    $  "Scheduled update to run in "
    ++ show pauseTimeMinutes
    ++ " minutes"
  threadDelay (pauseTimeMinutes * 60 * 1000 * 1000)
  job
  runScheduled job pauseTimeMinutes
