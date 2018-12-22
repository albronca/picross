module Main exposing (main)

import Browser exposing (sandbox)
import Element exposing (..)
import Html exposing (Html)


main : Program () Model Msg
main =
    sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    List Bool


init : Model
init =
    []



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    model



-- VIEW


view : Model -> Html Msg
view model =
    layout [] <| text "hello world"
