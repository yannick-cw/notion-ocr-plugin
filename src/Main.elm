module Main exposing (main)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Browser
import Html exposing (Html, button, div, h6, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Maybe exposing (withDefault)
import Ports exposing (askToken, notionToken)



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
    ( { token = Nothing, showNoTokenFound = False, syncState = SyncOff }, Cmd.none )



---- UPDATE ----


type Msg
    = GotToken (Maybe String)
    | AskToken
    | SyncState SyncState


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotToken (Just token) ->
            ( { model | token = Just token, showNoTokenFound = False }, Cmd.none )

        GotToken Nothing ->
            ( { model | showNoTokenFound = True, syncState = SyncOff }, Cmd.none )

        AskToken ->
            ( model, askToken () )

        SyncState SyncOff ->
            ( { model | syncState = SyncOff }, Cmd.none )

        SyncState SyncOn ->
            ( { model | syncState = SyncOn }, askToken () )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        errorMsg =
            if model.showNoTokenFound then
                [ Alert.simpleDanger [] [ text "Could not find your token, please log in to Notion and try again" ] ]

            else
                []

        runSyncBtn =
            div [] [ Button.button [ Button.primary, Button.onClick AskToken ] [ text "Run Once" ] ]

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



--div
--[]
--([ img [ src "/logo.svg" ] []
--, h1 [] [ text "Your Elm App is working!" ]
--, h1 [] [ text (withDefault "No token loaded" model.token) ]
--, button [ onClick AskToken ] [ text "Sync Notion" ]
--]
--++ errorMsg
--)
---- Subs ----


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
