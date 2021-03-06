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


getGetInitState : (Maybe String) -> (Result Http.Error  (InitState)  -> msg) -> Cmd msg
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
                Url.Builder.crossOrigin "http://64.227.2.193:8081"
                    [ "getInitState"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecInitState
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postRunOnce : (Maybe String) -> (Result Http.Error  (())  -> msg) -> Cmd msg
postRunOnce query_token toMsg =
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
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://64.227.2.193:8081"
                    [ "runOnce"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postSetSyncState : (Maybe String) -> SyncState -> (Result Http.Error  (())  -> msg) -> Cmd msg
postSetSyncState query_token body toMsg =
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
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://64.227.2.193:8081"
                    [ "setSyncState"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncSyncState body)
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
