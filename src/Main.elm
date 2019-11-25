module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Maybe exposing (withDefault)
import Ports exposing (askToken, notionToken)



---- MODEL ----


type alias Model =
    { token : Maybe String
    , showNoTokenFound : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { token = Nothing, showNoTokenFound = False }, Cmd.none )



---- UPDATE ----


type Msg
    = GotToken (Maybe String)
    | AskToken


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotToken (Just token) ->
            ( { model | token = Just token }, Cmd.none )

        GotToken Nothing ->
            ( { model | showNoTokenFound = True }, Cmd.none )

        AskToken ->
            ( model, askToken () )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        errorMsg =
            if model.showNoTokenFound then
                [ h1 [] [ text "Could not find your token, please log in to Notion and try again" ] ]

            else
                []
    in
    div []
        ([ img [ src "/logo.svg" ] []
         , h1 [] [ text "Your Elm App is working!" ]
         , h1 [] [ text (withDefault "No token loaded" model.token) ]
         , button [ onClick AskToken ] [ text "Sync Notion" ]
         ]
            ++ errorMsg
        )



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
