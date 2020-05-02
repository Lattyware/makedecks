module ManyDecks.Editor exposing (Model, Msg, init, update, view)

import FontAwesome.Icon as Icon
import FontAwesome.Layering as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Transforms as Icon
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Attributes.Aria as HtmlA
import Html.Events as HtmlE
import Html5.DragDrop as DragDrop
import List.Extra as List
import ManyDecks.Template as Template
import MassiveDecks.Card.Call as Call
import MassiveDecks.Card.Model as Card
import MassiveDecks.Card.Parts as Parts exposing (Part)
import MassiveDecks.Card.Response as Response
import MassiveDecks.Card.Source.Model as Source
import MassiveDecks.Model exposing (Shared)
import MassiveDecks.Pages.Lobby.Configure.Model as Config exposing (Config)
import MassiveDecks.Strings as Strings
import MassiveDecks.Strings.Languages as Lang
import MassiveDecks.Util.Html as Html


type alias Model =
    { editing : Card
    , index : Maybe Int
    , dragDrop : DragDrop.Model PartId PartId
    }


type Msg
    = ChangeName String
    | ChangeCard (Maybe Int) Card
    | Append Part
    | AppendLine
    | ChangeResponse String
    | Add Card
    | Edit Int Card
    | Delete Int
    | Revert Int
    | ChangePart PartId Part
    | SelectPart PartId
    | DeletePart PartId
    | DragDropMsg (DragDrop.Msg PartId PartId)


init : Model
init =
    { editing = C Nothing blankCall
    , index = Nothing
    , dragDrop = DragDrop.init
    }


update : Msg -> Template.Deck -> Model -> ( Template.Deck, Model, Cmd msg )
update msg deck model =
    case msg of
        ChangeName name ->
            ( { deck | name = name }, model, Cmd.none )

        ChangeCard index card ->
            ( deck, { model | editing = card, index = index }, Cmd.none )

        Append part ->
            let
                new =
                    case model.editing of
                        C selection (Template.Call lines) ->
                            let
                                ( selectionOrLast, added ) =
                                    case selection of
                                        Just s ->
                                            ( s, { s | part = s.part + 1 } )

                                        Nothing ->
                                            let
                                                next =
                                                    nextPartId lines
                                            in
                                            ( next, next )
                            in
                            lines
                                |> insertPartAfter selectionOrLast part
                                |> Template.Call
                                |> C (Just added)

                        R _ ->
                            model.editing
            in
            ( deck, { model | editing = new }, Cmd.none )

        AppendLine ->
            let
                new =
                    case model.editing of
                        C selection (Template.Call lines) ->
                            let
                                selectionOrLast =
                                    selection |> Maybe.map (\s -> { s | part = s.part + 1 }) |> Maybe.withDefault (nextPartId lines)

                                split index line =
                                    if index == selectionOrLast.line then
                                        let
                                            ( first, second ) =
                                                line |> List.splitAt selectionOrLast.part
                                        in
                                        [ first, second ]

                                    else
                                        [ line ]
                            in
                            C Nothing (Template.Call (lines |> List.indexedMap split |> List.concat))

                        R _ ->
                            model.editing
            in
            ( deck, { model | editing = new }, Cmd.none )

        ChangeResponse text ->
            let
                new =
                    case model.editing of
                        R _ ->
                            R (Template.Response text)

                        _ ->
                            model.editing
            in
            ( deck, { model | editing = new }, Cmd.none )

        Add card ->
            let
                newDeck =
                    case card of
                        C _ call ->
                            { deck | calls = simplify call :: deck.calls }

                        R response ->
                            { deck | responses = response :: deck.responses }
            in
            ( newDeck, { model | editing = card, index = Just 0 }, Cmd.none )

        Edit index card ->
            let
                newDeck =
                    case card of
                        C _ call ->
                            { deck | calls = deck.calls |> List.setAt index (simplify call) }

                        R response ->
                            { deck | responses = deck.responses |> List.setAt index response }
            in
            ( newDeck, model, Cmd.none )

        Delete index ->
            let
                newDeck =
                    case model.editing of
                        C _ _ ->
                            { deck | calls = deck.calls |> List.removeAt index }

                        R _ ->
                            { deck | responses = deck.responses |> List.removeAt index }
            in
            ( newDeck, { model | index = Nothing }, Cmd.none )

        Revert index ->
            let
                original =
                    case model.editing of
                        C _ _ ->
                            deck.calls |> List.getAt index |> Maybe.map (C Nothing)

                        R _ ->
                            deck.responses |> List.getAt index |> Maybe.map R
            in
            ( deck, { model | editing = original |> Maybe.withDefault model.editing }, Cmd.none )

        ChangePart id replacement ->
            let
                new =
                    case model.editing of
                        C selectedPart (Template.Call parts) ->
                            (parts |> replacePart id (always replacement))
                                |> Template.Call
                                |> C selectedPart

                        _ ->
                            model.editing
            in
            ( deck, { model | editing = new }, Cmd.none )

        SelectPart newSelection ->
            let
                new =
                    case model.editing of
                        C _ call ->
                            C (Just newSelection) call

                        _ ->
                            model.editing
            in
            ( deck, { model | editing = new }, Cmd.none )

        DeletePart part ->
            let
                new =
                    case model.editing of
                        C selection (Template.Call lines) ->
                            C selection (lines |> removePart part |> Template.Call)

                        _ ->
                            model.editing
            in
            ( deck, { model | editing = new }, Cmd.none )

        DragDropMsg dragDropMsg ->
            let
                ( dragDrop, result ) =
                    DragDrop.update dragDropMsg model.dragDrop

                new =
                    case model.editing of
                        C _ (Template.Call lines) ->
                            case result of
                                Just ( from, to, _ ) ->
                                    let
                                        swap fromPart toPart =
                                            lines |> replacePart from (always toPart) |> replacePart to (always fromPart)

                                        updated =
                                            Maybe.map2 swap
                                                (lines |> getPart from)
                                                (lines |> getPart to)
                                    in
                                    updated |> Maybe.map (Template.Call >> C (Just to))

                                Nothing ->
                                    Nothing

                        _ ->
                            Nothing
            in
            ( deck, { model | editing = new |> Maybe.withDefault model.editing, dragDrop = dragDrop }, Cmd.none )


view : (Msg -> msg) -> Shared -> Template.Deck -> Model -> Html msg
view wrap shared deck { editing, index, dragDrop } =
    let
        nameEditor =
            Html.label []
                [ Icon.viewIcon Icon.tag
                , Html.input
                    [ HtmlA.type_ "text"
                    , HtmlA.minlength 1
                    , HtmlA.value deck.name
                    , HtmlE.onInput (ChangeName >> wrap)
                    ]
                    []
                ]

        callListItem callIndex (Template.Call call) =
            Html.li
                [ HtmlE.onClick (call |> Template.Call |> C Nothing |> ChangeCard (Just callIndex) |> wrap)
                , HtmlA.classList [ ( "selected", index == Just callIndex && (editing |> isCall) ) ]
                ]
                [ call |> Parts.viewLinesString "_" [] |> String.join "⏎" |> Html.text ]

        responseListItem responseIndex (Template.Response response) =
            Html.li
                [ HtmlE.onClick (response |> Template.Response |> R |> ChangeCard (Just responseIndex) |> wrap)
                , HtmlA.classList [ ( "selected", index == Just responseIndex && not (editing |> isCall) ) ]
                ]
                [ Html.text response ]

        cards =
            Html.ul [ HtmlA.id "cards" ]
                [ Html.li [ HtmlA.id "calls" ]
                    [ Html.div [ HtmlA.class "top" ]
                        [ Html.h2 []
                            [ Strings.Plural { singular = Strings.Call, amount = Nothing } |> Lang.html shared ]
                        , Html.button [ blankCall |> C Nothing |> ChangeCard Nothing |> wrap |> HtmlE.onClick ]
                            [ Icon.viewIcon Icon.plus ]
                        ]
                    , Html.ul [] (deck.calls |> List.indexedMap callListItem)
                    ]
                , Html.li [ HtmlA.id "responses" ]
                    [ Html.div [ HtmlA.class "top" ]
                        [ Html.h2 []
                            [ Strings.Plural { singular = Strings.Response, amount = Nothing } |> Lang.html shared ]
                        , Html.button [ blankResponse |> R |> ChangeCard Nothing |> wrap |> HtmlE.onClick ]
                            [ Icon.viewIcon Icon.plus ]
                        ]
                    , Html.ul [] (deck.responses |> List.indexedMap responseListItem)
                    ]
                ]
    in
    Html.div [ HtmlA.id "editor" ] [ cardEditor wrap shared dragDrop deck index editing, nameEditor, cards ]



{- Private -}


type Card
    = C (Maybe PartId) Template.Call
    | R Template.Response


type alias PartId =
    { line : Int
    , part : Int
    }


simplify : Template.Call -> Template.Call
simplify (Template.Call lines) =
    let
        simplifyLine line =
            case line of
                (Parts.Text a styleA) :: (Parts.Text b styleB) :: rest ->
                    if styleA == styleB then
                        simplifyLine (Parts.Text (a ++ b) styleA :: rest)

                    else
                        Parts.Text a styleA :: Parts.Text b styleB :: (simplifyLine rest |> Maybe.withDefault []) |> Just

                first :: rest ->
                    first :: (simplifyLine rest |> Maybe.withDefault []) |> Just

                [] ->
                    Nothing
    in
    lines |> List.filterMap simplifyLine |> Template.Call


isValid : Card -> Bool
isValid card =
    case card of
        C _ (Template.Call lines) ->
            lines |> List.any (List.any Parts.isSlot)

        R (Template.Response _) ->
            True


nextPartId : List (List Part) -> PartId
nextPartId =
    let
        internal index parts =
            case parts of
                [] ->
                    { line = 0, part = 0 }

                last :: [] ->
                    { line = index, part = last |> List.length }

                _ :: rest ->
                    internal (index + 1) rest
    in
    internal 0


getPart : PartId -> List (List item) -> Maybe item
getPart { line, part } parts =
    parts |> List.getAt line |> Maybe.andThen (List.getAt part)


replacePart : PartId -> (item -> item) -> List (List item) -> List (List item)
replacePart { line, part } replacement parts =
    parts |> List.updateAt line (List.updateAt part replacement)


removePart : PartId -> List (List item) -> List (List item)
removePart { line, part } parts =
    parts |> List.updateAt line (List.removeAt part)


insertPartAfter : PartId -> item -> List (List item) -> List (List item)
insertPartAfter { line, part } insertion parts =
    parts |> List.updateAt line (insertAt part insertion)


insertAt : Int -> a -> List a -> List a
insertAt index item items =
    let
        ( start, end ) =
            List.splitAt (index + 1) items
    in
    start ++ [ item ] ++ end


isCall : Card -> Bool
isCall card =
    case card of
        C _ _ ->
            True

        R _ ->
            False


config : Config
config =
    Config.fake


blankCall : Template.Call
blankCall =
    Template.Call [ [] ]


blankResponse : Template.Response
blankResponse =
    Template.Response ""


cardEditor : (Msg -> msg) -> Shared -> DragDrop.Model PartId PartId -> Template.Deck -> Maybe Int -> Card -> Html msg
cardEditor wrap shared dragDrop deck index editing =
    Html.div [ HtmlA.id "card-editor" ]
        [ addControls wrap index editing
        , preview wrap shared deck.name dragDrop editing
        , deckControls wrap deck index editing
        ]


deckControls : (Msg -> msg) -> Template.Deck -> Maybe Int -> Card -> Html msg
deckControls wrap deck maybeIndex card =
    let
        { add, edit, revert, delete } =
            case maybeIndex of
                Just index ->
                    let
                        isChanged =
                            case card of
                                C _ call ->
                                    deck.calls |> List.getAt index |> Maybe.map ((/=) (simplify call)) |> Maybe.withDefault False

                                R response ->
                                    deck.responses |> List.getAt index |> Maybe.map ((/=) response) |> Maybe.withDefault False
                    in
                    { add = HtmlA.disabled True
                    , edit =
                        if isChanged && isValid card then
                            Edit index card |> wrap |> HtmlE.onClick

                        else
                            HtmlA.disabled True
                    , revert =
                        if isChanged then
                            Revert index |> wrap |> HtmlE.onClick

                        else
                            HtmlA.disabled True
                    , delete = Delete index |> wrap |> HtmlE.onClick
                    }

                Nothing ->
                    { add =
                        if isValid card then
                            Add card |> wrap |> HtmlE.onClick

                        else
                            HtmlA.disabled True
                    , edit = HtmlA.disabled True
                    , revert = HtmlA.disabled True
                    , delete = HtmlA.disabled True
                    }

        controls =
            [ Html.button [ add ] [ Icon.viewIcon Icon.plus ]
            , Html.button [ edit ] [ Icon.viewIcon Icon.save ]
            , Html.button [ revert ] [ Icon.viewIcon Icon.undo ]
            , Html.button [ delete ] [ Icon.viewIcon Icon.trash ]
            ]
    in
    Html.div [ HtmlA.id "deck-controls", HtmlA.class "controls" ] controls


addControls : (Msg -> msg) -> Maybe Int -> Card -> Html msg
addControls wrap index card =
    let
        controls =
            case card of
                C selectedPart (Template.Call lines) ->
                    [ Html.div [ HtmlA.id "add-controls", HtmlA.class "controls" ]
                        [ Html.button [ Parts.Text "Text" Parts.NoStyle |> Append |> wrap |> HtmlE.onClick ]
                            [ Icon.layers []
                                [ Icon.viewTransformed [] [ Icon.left 1 ] Icon.font
                                , Icon.viewTransformed [] [ Icon.shrink 4, Icon.down 3, Icon.right 9 ] Icon.plusCircle
                                ]
                            ]
                        , Html.button [ Parts.Slot Parts.NoTransform Parts.NoStyle |> Append |> wrap |> HtmlE.onClick ]
                            [ Icon.layers []
                                [ Icon.viewTransformed [] [ Icon.left 1 ] Icon.underline
                                , Icon.viewTransformed [] [ Icon.shrink 4, Icon.down 3, Icon.right 9 ] Icon.plusCircle
                                ]
                            ]
                        , Html.button [ AppendLine |> wrap |> HtmlE.onClick ]
                            [ Icon.layers []
                                [ Icon.viewTransformed [] [ Icon.rotate 90, Icon.up 1, Icon.left 1 ] Icon.levelDownAlt
                                , Icon.viewTransformed [] [ Icon.shrink 4, Icon.down 3, Icon.right 9 ] Icon.plusCircle
                                ]
                            ]
                        ]
                    , selectedPart |> Maybe.map (partEditor wrap lines) |> Maybe.withDefault Html.nothing
                    ]

                R (Template.Response text) ->
                    [ Html.textarea
                        [ HtmlE.onInput (\t -> ChangeCard index (R (Template.Response t)) |> wrap)
                        , HtmlA.value text
                        ]
                        []
                    ]
    in
    Html.div [ HtmlA.id "edit-controls", HtmlA.class "controls" ] controls


partEditor : (Msg -> msg) -> List (List Part) -> PartId -> Html msg
partEditor wrap parts id =
    let
        part =
            parts |> getPart id

        changePart =
            ChangePart id >> wrap

        delete =
            Html.button [ id |> DeletePart |> wrap |> HtmlE.onClick ] [ Icon.viewIcon Icon.trash ]
    in
    case part of
        Just (Parts.Text text style) ->
            Html.div [ HtmlA.class "text part-editor" ]
                [ Html.textarea
                    [ HtmlE.onInput (\t -> Parts.Text t style |> changePart)
                    , HtmlA.value text
                    ]
                    []
                , styleEditor style (Parts.Text text >> changePart)
                , delete
                ]

        Just (Parts.Slot transform style) ->
            Html.div [ HtmlA.class "slot part-editor" ]
                [ (Parts.Slot transform >> changePart) |> styleEditor style
                , (\t -> Parts.Slot t style) >> changePart |> transformEditor transform
                , delete
                ]

        Nothing ->
            Html.nothing


styleEditor : Parts.Style -> (Parts.Style -> msg) -> Html msg
styleEditor current change =
    let
        styles =
            [ ( Parts.Em, Icon.viewIcon Icon.italic ) ]
    in
    Html.div [ HtmlA.class "style toggles" ] (toggleButtons change current Parts.NoStyle styles)


transformEditor : Parts.Transform -> (Parts.Transform -> msg) -> Html msg
transformEditor current change =
    let
        transforms =
            [ ( Parts.Capitalize, Icon.text [] "Aa" ), ( Parts.UpperCase, Icon.text [] "AA" ) ]
    in
    Html.div [ HtmlA.class "transform toggles" ] (toggleButtons change current Parts.NoTransform transforms)


toggleButtons : (a -> msg) -> a -> a -> List ( a, Html msg ) -> List (Html msg)
toggleButtons change current off values =
    values |> List.map (toggleButton change current off)


toggleButton : (a -> msg) -> a -> a -> ( a, Html msg ) -> Html msg
toggleButton change current off ( target, icon ) =
    let
        ( changeTo, slash ) =
            if current == target then
                ( off, Icon.slash |> Icon.viewIcon |> Just )

            else
                ( target, Nothing )
    in
    Html.button [ HtmlE.onClick (changeTo |> change), current == target |> HtmlA.ariaPressed ]
        [ Icon.layers [] (List.filterMap identity [ Just icon, slash ])
        ]


preview : (Msg -> msg) -> Shared -> String -> DragDrop.Model PartId PartId -> Card -> Html msg
preview wrap shared deckName dragDrop card =
    let
        cardId =
            ""

        source =
            deckName |> Just |> Source.Fake
    in
    case card of
        C selected (Template.Call lines) ->
            let
                dragged =
                    dragDrop |> DragDrop.getDragId

                targeted =
                    dragDrop |> DragDrop.getDropId

                partAttribute line part _ =
                    let
                        id =
                            { line = line, part = part }
                    in
                    List.concat
                        [ [ id |> SelectPart |> wrap |> HtmlE.onClick
                          , HtmlA.classList
                                [ ( "selected", Just id == selected )
                                , ( "target", Just id == targeted )
                                , ( "dragged", Just id == dragged )
                                ]
                          ]
                        , DragDrop.draggable (DragDropMsg >> wrap) id
                        , DragDrop.droppable (DragDropMsg >> wrap) id
                        ]
            in
            Call.viewWithAttributes shared config Card.Front [] partAttribute [] (Card.call (lines |> Parts.unsafeFromList) cardId source)

        R (Template.Response response) ->
            Response.view shared config Card.Front [] (Card.response response cardId source)
