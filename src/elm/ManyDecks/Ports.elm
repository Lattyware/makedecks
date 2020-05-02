port module ManyDecks.Ports exposing (json5Error, parseJson5, parsedJson5)

import Json.Decode as Json


port parseJson5 : String -> Cmd msg


port parsedJson5 : (Json.Value -> msg) -> Sub msg


port json5Error : (String -> msg) -> Sub msg
