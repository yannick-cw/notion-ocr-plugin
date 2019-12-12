port module Ports exposing (askToken, notionToken)


port notionToken : (Maybe String -> msg) -> Sub msg


port askToken : () -> Cmd msg
