module ManyDecks.Loader exposing (Model, Msg, init, subscriptions, update, view)

import File exposing (File)
import File.Download
import File.Select
import FontAwesome.Icon as Icon
import FontAwesome.Solid as Icon
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as Json
import Json.Encode
import ManyDecks.Ports as Ports
import ManyDecks.Template as Template
import MassiveDecks.Card.Parts as Parts
import Task exposing (Task)


type alias Model =
    { open : Bool
    , calls : String
    , responses : String
    , errors : List String
    }


type Msg
    = Select
    | Load File
    | Parse String
    | Parsed Json.Value
    | Loaded Template.Deck
    | ToggleImportDialog
    | EditImport Textarea String
    | TryImport
    | Error String
    | Save


subscriptions : (Msg -> msg) -> Sub msg
subscriptions wrap =
    Sub.batch
        [ Parsed >> wrap |> Ports.parsedJson5
        , Error >> wrap |> Ports.json5Error
        ]


init : Model
init =
    { open = False
    , calls = ""
    , responses = ""
    , errors = []
    }


view : (Msg -> msg) -> Model -> Html msg
view wrap model =
    Html.div [ HtmlA.id "loader" ]
        [ Html.button
            [ HtmlE.onClick (Select |> wrap)
            ]
            [ Icon.viewIcon Icon.folderOpen ]
        , Html.button
            [ HtmlE.onClick (ToggleImportDialog |> wrap)
            ]
            [ Icon.viewIcon Icon.fileImport ]
        , Html.button
            [ HtmlE.onClick (Save |> wrap)
            ]
            [ Icon.viewIcon Icon.save ]
        , Html.ul [ HtmlA.class "errors" ] (model.errors |> List.map (\error -> Html.li [] [ Html.text error ]))
        , Html.div [ HtmlA.id "import", HtmlA.classList [ ( "open", model.open ) ] ]
            [ Html.div [ HtmlA.class "textareas" ]
                [ Html.textarea
                    [ HtmlA.value model.calls
                    , HtmlE.onInput (EditImport Call >> wrap)
                    , HtmlA.class "calls"
                    ]
                    []
                , Html.textarea
                    [ HtmlA.value model.responses
                    , HtmlE.onInput (EditImport Response >> wrap)
                    , HtmlA.class "responses"
                    ]
                    []
                ]
            , Html.button [ HtmlE.onClick (TryImport |> wrap) ] [ Icon.viewIcon Icon.fileImport ]
            ]
        ]


update : (Msg -> msg) -> Template.Deck -> Model -> Msg -> ( Template.Deck, Model, Cmd msg )
update wrap deck model msg =
    case msg of
        Select ->
            ( deck, model, File.Select.file [ extension ] (Load >> wrap) )

        Load file ->
            ( deck, model, file |> File.toString |> Task.perform (Parse >> wrap) )

        Parse rawFile ->
            ( deck, model, Ports.parseJson5 rawFile )

        Parsed json ->
            case json |> Json.decodeValue Template.decode of
                Ok newDeck ->
                    ( newDeck, model, Cmd.none )

                Err error ->
                    ( deck, { model | errors = Json.errorToString error :: model.errors }, Cmd.none )

        Loaded newDeck ->
            ( newDeck, model, Cmd.none )

        Save ->
            let
                id =
                    deck.name |> String.words |> String.join "-" |> String.toLower
            in
            ( deck
            , model
            , deck
                |> Template.encode
                |> Json.Encode.encode 2
                |> File.Download.string (id ++ extension) mimeType
            )

        ToggleImportDialog ->
            ( deck, { model | open = not model.open }, Cmd.none )

        EditImport textarea text ->
            let
                new =
                    case textarea of
                        Call ->
                            { model | calls = text }

                        Response ->
                            { model | responses = text }
            in
            ( deck, new, Cmd.none )

        TryImport ->
            let
                parseCall line =
                    let
                        appendSlotIfNone lines =
                            if lines |> List.any (List.any Parts.isSlot) then
                                lines

                            else
                                lines ++ [ [ Parts.Slot Parts.Capitalize Parts.NoStyle, Parts.Text "." Parts.NoStyle ] ]
                    in
                    case line |> String.trim |> String.split "<br>" |> List.map (String.split "_") of
                        [] ->
                            Nothing

                        [ [] ] ->
                            Nothing

                        other ->
                            other
                                |> List.map
                                    (List.map (\t -> Parts.Text t Parts.NoStyle) >> addSlots)
                                |> appendSlotIfNone
                                |> Template.Call
                                |> Just

                parseResponse line =
                    case String.trim line of
                        "" ->
                            Nothing

                        other ->
                            other |> Template.Response |> Just

                calls =
                    model.calls |> String.lines |> List.filterMap parseCall

                responses =
                    model.responses |> String.lines |> List.filterMap parseResponse
            in
            ( { deck
                | calls = calls ++ deck.calls
                , responses = responses ++ deck.responses
              }
            , { model | open = False, calls = "", responses = "" }
            , Cmd.none
            )

        Error error ->
            ( deck, { model | errors = error :: model.errors }, Cmd.none )



{- Private -}


addSlots : List Parts.Part -> List Parts.Part
addSlots parts =
    case parts of
        [] ->
            []

        (Parts.Text "" _) :: next :: rest ->
            Parts.Slot Parts.Capitalize Parts.NoStyle :: addSlots (next :: rest)

        current :: next :: rest ->
            let
                transform =
                    case current of
                        Parts.Text text _ ->
                            let
                                t =
                                    text |> String.trimRight |> String.right 1
                            in
                            if t == "." || t == "!" || t == "?" then
                                Parts.Capitalize

                            else
                                Parts.NoTransform

                        _ ->
                            Parts.NoTransform
            in
            current :: Parts.Slot transform Parts.NoStyle :: addSlots (next :: rest)

        last :: [] ->
            [ last ]


type Textarea
    = Call
    | Response


extension : String
extension =
    ".deck.json5"


mimeType : String
mimeType =
    "application/mddeck+json5"
