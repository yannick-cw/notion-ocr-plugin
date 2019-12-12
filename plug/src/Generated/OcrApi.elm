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

type alias InitState  = SyncState

jsonDecInitState : Json.Decode.Decoder ( InitState )
jsonDecInitState =
    jsonDecSyncState

jsonEncInitState : InitState -> Value
jsonEncInitState  val = jsonEncSyncState val



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


getGetInitState : (Maybe String) -> (Result Http.Error  ((Maybe SyncState))  -> msg) -> Cmd msg
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
                Url.Builder.crossOrigin ""
                    [ "getInitState"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.maybe (jsonDecSyncState))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
