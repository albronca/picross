module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown, onKeyUp, onResize)
import Element exposing (..)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Json.Decode as Decode
import List.Extra
import Matrix exposing (Matrix)
import Random


main : Program WindowSize Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { board : Matrix CellState
    , solution : Matrix Bool
    , shiftPressed : Bool
    , gameState : GameState
    , windowSize : WindowSize
    }


type alias WindowSize =
    { width : Int
    , height : Int
    }


type GameState
    = Setup
    | Playing
    | Won


type CellState
    = Empty
    | Filled
    | Flagged


initialModel : WindowSize -> Model
initialModel windowSize =
    { board = Matrix.repeat 5 5 Empty
    , solution = Matrix.repeat 5 5 False
    , shiftPressed = False
    , gameState = Setup
    , windowSize = windowSize
    }


init : WindowSize -> ( Model, Cmd Msg )
init windowSize =
    ( initialModel windowSize, Cmd.none )



-- UPDATE


type Msg
    = ToggleCell Int Int
    | SolutionGenerated (List ( Int, Int ))
    | KeyDown String
    | KeyUp String
    | NewGame
    | WindowResize Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        ToggleCell x y ->
            let
                toggleFunction =
                    if model.shiftPressed then
                        flagCell

                    else
                        fillCell

                newBoard =
                    toggleFunction ( x, y ) model.board

                newGameState =
                    getGameState { model | board = newBoard }
            in
            ( { model
                | board = newBoard
                , gameState = newGameState
              }
            , Cmd.none
            )

        SolutionGenerated coordList ->
            ( { model
                | solution = toggleCells coordList model.solution
                , gameState = Playing
              }
            , Cmd.none
            )

        KeyDown key ->
            case key of
                "Shift" ->
                    ( { model
                        | shiftPressed = True
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        KeyUp key ->
            case key of
                "Shift" ->
                    ( { model
                        | shiftPressed = False
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        NewGame ->
            ( { model | board = Matrix.repeat 5 5 Empty }
            , Random.generate SolutionGenerated (coordListGenerator model.solution)
            )

        WindowResize width height ->
            ( { model | windowSize = { width = width, height = height } }, Cmd.none )


getGameState : Model -> GameState
getGameState model =
    if boardMatchesSolution model then
        Won

    else
        Playing


boardMatchesSolution : Model -> Bool
boardMatchesSolution model =
    let
        filledCells =
            Matrix.map (\cellState -> cellState == Filled) model.board
    in
    filledCells == model.solution


fillCell : ( Int, Int ) -> Matrix CellState -> Matrix CellState
fillCell ( toggleX, toggleY ) =
    Matrix.indexedMap
        (\cellX cellY cellState ->
            if cellX == toggleX && cellY == toggleY then
                case cellState of
                    Empty ->
                        Filled

                    Filled ->
                        Empty

                    Flagged ->
                        Flagged

            else
                cellState
        )


flagCell : ( Int, Int ) -> Matrix CellState -> Matrix CellState
flagCell ( toggleX, toggleY ) =
    Matrix.indexedMap
        (\cellX cellY cellState ->
            if cellX == toggleX && cellY == toggleY then
                case cellState of
                    Empty ->
                        Flagged

                    Filled ->
                        Filled

                    Flagged ->
                        Empty

            else
                cellState
        )


toggleCells : List ( Int, Int ) -> Matrix Bool -> Matrix Bool
toggleCells coordList board =
    coordList
        |> List.foldl toggleCell board


toggleCell : ( Int, Int ) -> Matrix Bool -> Matrix Bool
toggleCell ( toggleX, toggleY ) =
    Matrix.indexedMap
        (\cellX cellY isOn ->
            if cellX == toggleX && cellY == toggleY then
                not isOn

            else
                isOn
        )


coordListGenerator : Matrix a -> Random.Generator (List ( Int, Int ))
coordListGenerator matrix =
    let
        ( width, height ) =
            ( Matrix.width matrix, Matrix.height matrix )

        ( maxX, maxY ) =
            ( width - 1, height - 1 )

        maxCount =
            width * height
    in
    Random.int 5 maxCount
        |> Random.andThen (\len -> Random.list len (coordGenerator maxX maxY))
        |> Random.map List.Extra.unique


coordGenerator : Int -> Int -> Random.Generator ( Int, Int )
coordGenerator maxX maxY =
    Random.pair (Random.int 0 maxX) (Random.int 0 maxY)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown <| keyDecoder KeyDown
        , onKeyUp <| keyDecoder KeyUp
        ]


keyDecoder : (String -> Msg) -> Decode.Decoder Msg
keyDecoder msgConstructor =
    Decode.map msgConstructor (Decode.field "key" Decode.string)



-- VIEW


view : Model -> Html Msg
view model =
    layout [] <|
        gameView model


gameView : Model -> Element Msg
gameView model =
    model.board
        |> getRows
        |> List.indexedMap gameBoardRow
        |> column
            [ centerX
            , centerY
            , width <| px 500
            , height <| px 500
            , spacing 4
            , Font.size 12
            , onLeft <| rowHints model.solution
            , above <| columnHints model.solution
            , inFront <|
                menu model
            ]


menu : Model -> Element Msg
menu model =
    case model.gameState of
        Playing ->
            none

        _ ->
            column
                [ centerX
                , centerY
                , width <| px 500
                , height <| px 500
                , Background.color (rgba 255 255 255 0.8)
                , Font.size 24
                ]
                [ el
                    [ centerX, centerY ]
                    (text
                        (if model.gameState == Setup then
                            "picross"

                         else
                            "wow good job"
                        )
                    )
                , Input.button [ centerX, centerY ]
                    { onPress = Just NewGame
                    , label = text "New Game"
                    }
                ]


gameBoardRow : Int -> List CellState -> Element Msg
gameBoardRow y =
    List.indexedMap (gameBoardCell y)
        >> row [ width fill, height fill, centerX, spacing 4 ]


gameBoardCell : Int -> Int -> CellState -> Element Msg
gameBoardCell y x cellState =
    el
        [ Background.color <| cellColor cellState
        , width fill
        , height fill
        , Events.onClick <| ToggleCell x y
        ]
        none


cellColor : CellState -> Color
cellColor cellState =
    case cellState of
        Empty ->
            rgb255 200 200 200

        Filled ->
            rgb255 0 0 0

        Flagged ->
            rgb255 255 0 0


rowHints : Matrix Bool -> Element Msg
rowHints solution =
    solution
        |> getRows
        |> List.map List.Extra.group
        |> List.map (List.filter Tuple.first)
        |> List.map (List.map Tuple.second)
        |> List.map (List.map List.length)
        |> List.map (List.map ((+) 1))
        |> List.map (List.map String.fromInt)
        |> List.map (List.map text)
        |> List.map (List.map <| el [ alignRight ])
        |> List.map (row [ height fill, spacing 8 ])
        |> column [ height <| px 500, spacing 4, paddingXY 20 0 ]


columnHints : Matrix Bool -> Element Msg
columnHints solution =
    solution
        |> getColumns
        |> List.map List.Extra.group
        |> List.map (List.filter Tuple.first)
        |> List.map (List.map Tuple.second)
        |> List.map (List.map List.length)
        |> List.map (List.map ((+) 1))
        |> List.map (List.map String.fromInt)
        |> List.map (List.map text)
        |> List.map (List.map <| el [ centerX ])
        |> List.map (column [ width fill, spacing 8 ])
        |> row [ width <| px 500, spacing 4, centerX, paddingXY 0 20 ]



-- UTILITY


getRows : Matrix a -> List (List a)
getRows matrix =
    List.range 0 (Matrix.height matrix - 1)
        |> List.map (\y -> Matrix.getRow y matrix)
        |> List.map (Result.withDefault Array.empty)
        |> List.map Array.toList


getColumns : Matrix a -> List (List a)
getColumns matrix =
    List.range 0 (Matrix.width matrix - 1)
        |> List.map (\x -> Matrix.getColumn x matrix)
        |> List.map (Result.withDefault Array.empty)
        |> List.map Array.toList
