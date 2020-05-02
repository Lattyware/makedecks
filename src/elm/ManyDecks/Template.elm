module ManyDecks.Template exposing (Call(..), Card(..), Deck, Response(..), decode, encode)

import Json.Decode as Json
import Json.Encode
import MassiveDecks.Card.Parts as Parts exposing (Part(..))
import MassiveDecks.Models.Decoders as Decoders


type alias Deck =
    { name : String
    , calls : List Call
    , responses : List Response
    }


type Card
    = C Call
    | R Response


type Call
    = Call (List (List Part))


type Response
    = Response String


decode : Json.Decoder Deck
decode =
    Json.map3 Deck
        (Json.field "name" Json.string)
        (Json.field "calls" (decodeCall |> Json.list))
        (Json.field "responses" (decodeResponse |> Json.list))


decodeCall : Json.Decoder Call
decodeCall =
    Json.map Call
        (Json.list (Json.list Decoders.part))


decodeResponse : Json.Decoder Response
decodeResponse =
    Json.string |> Json.map Response


encode : Deck -> Json.Value
encode { name, calls, responses } =
    Json.Encode.object
        [ ( "name", name |> Json.Encode.string )
        , ( "calls", calls |> Json.Encode.list encodeCall )
        , ( "responses", responses |> Json.Encode.list encodeResponse )
        ]


encodeResponse : Response -> Json.Value
encodeResponse (Response response) =
    Json.Encode.string response


encodeCall : Call -> Json.Value
encodeCall (Call lines) =
    lines |> Json.Encode.list (encodePart |> Json.Encode.list)


encodePart : Part -> Json.Value
encodePart part =
    case part of
        Text text Parts.NoStyle ->
            text |> Json.Encode.string

        Text text style ->
            Json.Encode.object
                ([ Just ( "text", text |> Json.Encode.string )
                 , style |> encodeStyle
                 ]
                    |> List.filterMap identity
                )

        Slot transform style ->
            Json.Encode.object
                ([ transform |> encodeTransform
                 , style |> encodeStyle
                 ]
                    |> List.filterMap identity
                )


encodeStyle : Parts.Style -> Maybe ( String, Json.Value )
encodeStyle style =
    let
        name =
            case style of
                Parts.NoStyle ->
                    Nothing

                Parts.Em ->
                    Just "Em"
    in
    name |> Maybe.map (\n -> ( "style", n |> Json.Encode.string ))


encodeTransform : Parts.Transform -> Maybe ( String, Json.Value )
encodeTransform transform =
    let
        name =
            case transform of
                Parts.NoTransform ->
                    Nothing

                Parts.Capitalize ->
                    Just "Capitalize"

                Parts.UpperCase ->
                    Just "UpperCase"
    in
    name |> Maybe.map (\n -> ( "transform", n |> Json.Encode.string ))
