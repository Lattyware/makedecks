module ManyDecks exposing (main)

import Browser
import Browser.Navigation as Navigation
import FontAwesome.Styles as Icon
import Html
import Html.Attributes as HtmlA
import ManyDecks.Editor as Editor
import ManyDecks.Loader as Loader
import ManyDecks.Msg exposing (Msg(..))
import ManyDecks.Template as Template
import MassiveDecks.Cast.Model as Cast
import MassiveDecks.Model exposing (Shared)
import MassiveDecks.Notifications as Notifications
import MassiveDecks.Settings as Settings
import MassiveDecks.Speech as Speech
import MassiveDecks.Strings.Languages as Lang
import MassiveDecks.Util.Url as Url
import Url exposing (Url)


type alias Model =
    { shared : Shared
    , deck : Template.Deck
    , editor : Editor.Model
    , loader : Loader.Model
    }


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init () url key =
    ( { shared = shared url key
      , editor = Editor.init
      , loader = Loader.init
      , deck = { name = "New Deck", calls = [], responses = [] }
      }
    , Cmd.none
    )


shared : Url -> Navigation.Key -> Shared
shared url key =
    let
        ( speech, _ ) =
            Speech.init
    in
    { language = Lang.defaultLanguage
    , key = key
    , origin = Url.origin url
    , settings = { settings = Settings.defaults, open = False }
    , browserLanguage = Nothing
    , castStatus = Cast.NoDevicesAvailable
    , speech = speech
    , notifications = Notifications.init
    , remoteMode = False
    , sources = { builtIn = Nothing, cardcast = False }
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Loader.subscriptions LoaderMsg


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest _ =
    NoOp


onUrlChange : Url -> Msg
onUrlChange _ =
    NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoaderMsg loaderMsg ->
            let
                ( deck, loader, cmd ) =
                    Loader.update LoaderMsg model.deck model.loader loaderMsg
            in
            ( { model | deck = deck, loader = loader }, cmd )

        EditorMsg editorMsg ->
            let
                ( deck, editor, cmd ) =
                    Editor.update editorMsg model.deck model.editor
            in
            ( { model | deck = deck, editor = editor }, cmd )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Make Decks"
    , body =
        [ Html.div [ HtmlA.id "root" ]
            [ Icon.css
            , Loader.view LoaderMsg model.loader
            , Editor.view EditorMsg model.shared model.deck model.editor
            ]
        ]
    }
