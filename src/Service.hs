module Service
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
import           Control.Monad.Reader           ( runReaderT )
import           ScheduledRun
import           AppM                           ( AppM(..)
                                                , State(..)
                                                )
import qualified Data.Text.IO                  as TIO
                                                ( putStrLn )
import           Control.Concurrent.STM.TVar    ( newTVar )
import           Control.Monad.STM              ( atomically )


someFunc :: IO ()
someFunc = do
  tvar <- atomically (newTVar [])
  let s = State tvar
  bracket (forkIO $ run 8081 (app s)) killThread $ \_ -> runScheduled
    (   either TIO.putStrLn (const $ return ())
    =<< runReaderT (runExceptT (unwrap updateAll)) s
    )
    1

runScheduled :: IO () -> Int -> IO ()
runScheduled job pauseTimeMinutes = do
  putStrLn
    $  "Scheduled update to run in "
    ++ show pauseTimeMinutes
    ++ " minutes"
  threadDelay (pauseTimeMinutes * 60 * 1000 * 1000)
  job
  runScheduled job pauseTimeMinutes
