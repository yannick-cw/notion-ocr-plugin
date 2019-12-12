{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Http.Api where

import           Data.Text
import           Data.Time                      ( UTCTime )
import           Servant.API
import           Data.Aeson                     ( ToJSON )
import           GHC.Generics
import           Elm.Derive                     ( defaultOptions
                                                , deriveBoth
                                                )

type OcrApi
  = "getInitState" :> QueryParam "token" String :> Get '[JSON] (Maybe InitState)

newtype InitState = InitState { syncState :: SyncState } deriving (Eq, Show, Generic)
data SyncState = SyncOn | SyncOff deriving (Eq, Show, Generic)

deriveBoth defaultOptions ''InitState
deriveBoth defaultOptions ''SyncState



