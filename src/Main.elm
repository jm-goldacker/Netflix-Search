module Main exposing (..)

import Html
import View 
import Update
import Types exposing (Msg(..), Model, defaultModel)


main : Program Never Model Msg
main =
    Html.program
        { init = (defaultModel, Cmd.none)
        , view = View.view
        , update = Update.update
        , subscriptions = (\model -> Sub.none)
        }