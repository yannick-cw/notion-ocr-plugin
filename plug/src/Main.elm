module Main exposing (main)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Browser
import Generated.OcrApi as OcrApi exposing (getGetInitState, postRunOnce, postSetSyncState)
import Html exposing (Html, button, div, h6, text)
import Html.Events exposing (onClick)
import Http
import Ports exposing (askToken, notionToken)



-- TODO
-- error handling, when token can not be loaded, run fails, sync status?
-- styling
-- additional information link
-- Rate limit single runs e.g. (3/100) in February
-- Next scheduled run e.g. Syncing in 50 minutes
-- synced images / and used limit of images per month
---- MODEL ----


type alias Model =
    { token : Maybe String
    , showNoTokenFound : Bool
    , syncState : SyncState
    }


type SyncState
    = SyncOff
    | SyncOn


init : ( Model, Cmd Msg )
init =
    ( { token = Nothing, showNoTokenFound = False, syncState = SyncOff }, askToken () )



---- UPDATE ----


type Msg
    = GotToken (Maybe String)
    | RunOnce
    | SyncState SyncState
    | GotSyncState (Result Http.Error OcrApi.InitState)
    | SetSyncStateRes (Result Http.Error ())
    | RunOnceRes (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotToken (Just token) ->
            ( { model | token = Just token, showNoTokenFound = False }, getGetInitState (Just token) GotSyncState )

        GotToken Nothing ->
            ( { model | showNoTokenFound = True, syncState = SyncOff }, Cmd.none )

        RunOnce ->
            case model.token of
                Just c ->
                    ( model, postRunOnce (Just c) RunOnceRes )

                Nothing ->
                    -- Erro Msg here
                    ( model, Cmd.none )

        SyncState syncState ->
            case model.token of
                Just c ->
                    ( { model | syncState = syncState }, postSetSyncState (Just c) (syncStateToApiSync syncState) SetSyncStateRes )

                Nothing ->
                    -- Erro Msg here
                    ( { model | syncState = SyncOff }, Cmd.none )

        GotSyncState (Ok (OcrApi.InitState initState)) ->
            ( { model | syncState = apiSyncToSyncState initState.syncState }, Cmd.none )

        GotSyncState _ ->
            ( { model | syncState = SyncOff }, Cmd.none )

        SetSyncStateRes _ ->
            -- err or success msg here
            ( model, Cmd.none )

        RunOnceRes _ ->
            -- err or success msg here
            ( model, Cmd.none )


syncStateToApiSync : SyncState -> OcrApi.SyncState
syncStateToApiSync ss =
    case ss of
        SyncOn ->
            OcrApi.SyncOn

        SyncOff ->
            OcrApi.SyncOff


apiSyncToSyncState : OcrApi.SyncState -> SyncState
apiSyncToSyncState ss =
    case ss of
        OcrApi.SyncOn ->
            SyncOn

        OcrApi.SyncOff ->
            SyncOff



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        errorMsg =
            if model.showNoTokenFound then
                [ Alert.simpleDanger [] [ text "Can not find your token, please log-in to Notion and try again" ] ]

            else
                []

        runSyncBtn =
            div [] [ Button.button [ Button.primary, Button.onClick RunOnce ] [ text "Run Once" ] ]

        toggleBtns =
            div []
                (ButtonGroup.radioButtonGroup []
                    [ ButtonGroup.radioButton
                        (model.syncState == SyncOff)
                        [ if model.syncState == SyncOff then
                            Button.success

                          else
                            Button.secondary
                        , Button.onClick <| SyncState SyncOff
                        ]
                        [ text "Sync Off" ]
                    , ButtonGroup.radioButton
                        (model.syncState == SyncOn)
                        [ if model.syncState == SyncOn then
                            Button.success

                          else
                            Button.secondary
                        , Button.onClick <| SyncState SyncOn
                        ]
                        [ text "Sync On" ]
                    ]
                    :: errorMsg
                )
    in
    Grid.container []
        [ CDN.stylesheet
        , Grid.row []
            [ Grid.col []
                [ h6 [] [ text "Notion Image Search" ]
                , runSyncBtn
                , toggleBtns
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    notionToken GotToken



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
