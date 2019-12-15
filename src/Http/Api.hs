{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Http.Api where

import           Servant.API
import           Data.Aeson.Types               ( Options(..) )
import           GHC.Generics
import           Elm.Derive                     ( defaultOptions
                                                , deriveBoth
                                                )

type OcrApi
  = "getInitState" :> QueryParam "token" String :> Get '[JSON] (Maybe InitState) 
  :<|> "runOnce" :> QueryParam "token" String :> Post  '[JSON] () 
  :<|> "setSyncState" :> QueryParam "token" String :> ReqBody '[JSON] SyncState :> Post '[JSON] ()

newtype InitState = InitState { syncState :: SyncState } deriving (Eq, Show, Generic)
data SyncState = SyncOn | SyncOff deriving (Eq, Show, Generic)

deriveBoth ( defaultOptions { unwrapUnaryRecords = False }) ''InitState
deriveBoth defaultOptions ''SyncState



