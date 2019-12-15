module Generated.OcrApi exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set
import Http
import String
import Url.Builder

maybeBoolToIntStr : Maybe Bool -> String
maybeBoolToIntStr mx =
  case mx of
    Nothing -> ""
    Just True -> "1"
    Just False -> "0"

type InitState  = InitState
   { syncState: SyncState
   }

jsonDecInitState : Json.Decode.Decoder ( InitState )
jsonDecInitState =
   Json.Decode.succeed (\psyncState -> (InitState {syncState = psyncState}))
   |> required "syncState" (jsonDecSyncState)

jsonEncInitState : InitState -> Value
jsonEncInitState  (InitState val) =
   Json.Encode.object
   [ ("syncState", jsonEncSyncState val.syncState)
   ]



type SyncState  =
    SyncOn 
    | SyncOff 

jsonDecSyncState : Json.Decode.Decoder ( SyncState )
jsonDecSyncState = 
    let jsonDecDictSyncState = Dict.fromList [("SyncOn", SyncOn), ("SyncOff", SyncOff)]
    in  decodeSumUnaries "SyncState" jsonDecDictSyncState

jsonEncSyncState : SyncState -> Value
jsonEncSyncState  val =
    case val of
        SyncOn -> Json.Encode.string "SyncOn"
        SyncOff -> Json.Encode.string "SyncOff"


getGetInitState : (Maybe String) -> (Result Http.Error  ((Maybe InitState))  -> msg) -> Cmd msg
getGetInitState query_token toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [ [ query_token
                    |> Maybe.map (Url.Builder.string "token") ]
                ])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://localhost:8081"
                    [ "getInitState"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.maybe (jsonDecInitState))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
