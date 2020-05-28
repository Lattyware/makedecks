module MakeDecks.Msg exposing (Msg(..))

import MakeDecks.Editor as Editor
import MakeDecks.Loader as Loader


type Msg
    = EditorMsg Editor.Msg
    | LoaderMsg Loader.Msg
    | NoOp
