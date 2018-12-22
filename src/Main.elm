module Main exposing (main)

import Array exposing (Array)
import Browser exposing (sandbox)
import Element exposing (..)
import Element.Background as Background
import Element.Events as Events
import Html exposing (Html)
import Matrix exposing (Matrix)


main : Program () Model Msg
main =
    sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    Matrix Bool


init : Model
init =
    Matrix.repeat 5 5 False



-- UPDATE


type Msg
    = ToggleCell Int Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleCell x y ->
            toggleCell x y model


toggleCell : Int -> Int -> Model -> Model
toggleCell toggleX toggleY =
    Matrix.indexedMap
        (\cellX cellY isOn ->
            if cellX == toggleX && cellY == toggleY then
                not isOn

            else
                isOn
        )



-- VIEW


view : Model -> Html Msg
view model =
    layout [] <|
        gameBoard model


gameBoard : Model -> Element Msg
gameBoard model =
    model
        |> getRows
        |> List.indexedMap gameBoardRow
        |> column
            [ centerX
            , centerY
            , spacing 4
            , paddingXY 32 0
            ]


gameBoardRow : Int -> List Bool -> Element Msg
gameBoardRow y =
    List.indexedMap (gameBoardCell y) >> row [ centerX, spacing 4 ]


gameBoardCell : Int -> Int -> Bool -> Element Msg
gameBoardCell y x isOn =
    el
        [ Background.color <| cellColor isOn
        , width <| px 100
        , height <| px 100
        , Events.onClick <| ToggleCell x y
        ]
        none


cellColor : Bool -> Color
cellColor isOn =
    if isOn then
        rgb255 0 0 0

    else
        rgb255 200 200 200



-- UTILITY


getRows : Matrix a -> List (List a)
getRows matrix =
    List.range 0 (Matrix.height matrix - 1)
        |> List.map (\y -> Matrix.getRow y matrix)
        |> List.map (Result.withDefault Array.empty)
        |> List.map Array.toList
