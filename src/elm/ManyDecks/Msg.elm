module ManyDecks.Msg exposing (Msg(..))

import ManyDecks.Editor as Editor
import ManyDecks.Loader as Loader


type Msg
    = EditorMsg Editor.Msg
    | LoaderMsg Loader.Msg
    | NoOp
