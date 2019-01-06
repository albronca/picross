module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown, onKeyUp, onResize)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import List.Extra
import Matrix exposing (Matrix)
import Puzzle exposing (Puzzle, PuzzleSize(..), empty)
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
    , rowHints : List (List Hint)
    , columnHints : List (List Hint)
    , puzzleSize : PuzzleSize
    , shiftPressed : Bool
    , gameState : GameState
    , windowSize : WindowSize
    }


type alias Hint =
    { groupSize : Int
    , used : Bool
    }


type alias WindowSize =
    { width : Int
    , height : Int
    }


type GameState
    = MainMenu
    | Paused
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
    , rowHints = []
    , columnHints = []
    , puzzleSize = Small
    , shiftPressed = False
    , gameState = MainMenu
    , windowSize = windowSize
    }


init : WindowSize -> ( Model, Cmd Msg )
init windowSize =
    ( initialModel windowSize, Cmd.none )



-- UPDATE


type Msg
    = ClearBoard
    | KeyDown String
    | KeyUp String
    | GenerateRandomGame
    | ResumeGame
    | ReturnToMainMenu
    | SelectPuzzleSize PuzzleSize
    | SolutionGenerated (Matrix Bool)
    | ToggleCell Int Int
    | WindowResize Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClearBoard ->
            let
                clearedBoard =
                    model.board |> Matrix.map (always Empty)
            in
            ( { model | board = clearedBoard }, Cmd.none )

        KeyDown key ->
            case Debug.log "key" key of
                "Shift" ->
                    ( { model | shiftPressed = True }, Cmd.none )

                "Enter" ->
                    let
                        newGameState =
                            if model.gameState == Playing then
                                Paused

                            else
                                Playing
                    in
                    ( { model | gameState = newGameState }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        KeyUp key ->
            case key of
                "Shift" ->
                    ( { model | shiftPressed = False }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GenerateRandomGame ->
            ( model
            , Puzzle.solutionGenerator model.puzzleSize |> Random.generate SolutionGenerated
            )

        ResumeGame ->
            ( { model | gameState = Playing }, Cmd.none )

        ReturnToMainMenu ->
            ( { model | gameState = MainMenu }, Cmd.none )

        SelectPuzzleSize puzzleSize ->
            ( { model | puzzleSize = puzzleSize }, Cmd.none )

        SolutionGenerated solution ->
            let
                solutionWidth =
                    Matrix.width solution

                solutionHeight =
                    Matrix.height solution
            in
            ( { model
                | solution = solution
                , board = Matrix.repeat solutionWidth solutionHeight Empty
                , rowHints = getRowHints solution
                , columnHints = getColumnHints solution
                , gameState = Playing
              }
            , Cmd.none
            )

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

        WindowResize width height ->
            ( { model | windowSize = { width = width, height = height } }, Cmd.none )


getGameState : Model -> GameState
getGameState model =
    if boardMatchesSolution model then
        Won

    else
        Playing


getRowHints : Matrix Bool -> List (List Hint)
getRowHints solution =
    solution
        |> Matrix.rows
        |> List.map Array.toList
        |> List.map List.Extra.group
        |> List.map (List.filter Tuple.first)
        |> List.map (List.map Tuple.second)
        |> List.map (List.map List.length)
        |> List.map (List.map ((+) 1))
        |> List.map
            (List.map
                (\groupSize -> { groupSize = groupSize, used = False })
            )


getColumnHints : Matrix Bool -> List (List Hint)
getColumnHints solution =
    solution
        |> Matrix.columns
        |> List.map Array.toList
        |> List.map List.Extra.group
        |> List.map (List.filter Tuple.first)
        |> List.map (List.map Tuple.second)
        |> List.map (List.map List.length)
        |> List.map (List.map ((+) 1))
        |> List.map
            (List.map
                (\groupSize -> { groupSize = groupSize, used = False })
            )


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.gameState == Playing || model.gameState == Paused then
        Sub.batch
            [ onKeyDown <| keyDecoder KeyDown
            , onKeyUp <| keyDecoder KeyUp
            ]

    else
        Sub.none


keyDecoder : (String -> Msg) -> Decoder Msg
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
        |> Matrix.rows
        |> List.map Array.toList
        |> List.indexedMap gameBoardRow
        |> column
            [ centerX
            , centerY
            , width <| px 500
            , height <| px 500
            , Font.size 12
            , onLeft <| rowHints model.rowHints
            , above <| columnHints model.columnHints
            , inFront <|
                menu model
            ]


menu : Model -> Element Msg
menu model =
    case model.gameState of
        Playing ->
            none

        Paused ->
            column
                [ centerX
                , centerY
                , width <| px 500
                , height <| px 500
                , Background.color (rgba 255 255 255 0.8)
                , Font.size 24
                ]
                [ el [ centerX, centerY ] (text "PAUSED")
                , Input.button [ centerX, centerY ]
                    { onPress = Just ResumeGame
                    , label = text "Resume Game"
                    }
                , Input.button [ centerX, centerY ]
                    { onPress = Just ClearBoard
                    , label = text "Clear Board"
                    }
                , Input.button [ centerX, centerY ]
                    { onPress = Just ReturnToMainMenu
                    , label = text "Main Menu"
                    }
                ]

        MainMenu ->
            column
                [ centerX
                , centerY
                , width <| px 500
                , height <| px 500
                , Background.color (rgba 255 255 255 0.8)
                , Font.size 24
                ]
                [ el [ centerX, centerY ] (text "picross")
                , Input.radio
                    [ padding 10
                    , spacing 20
                    ]
                    { onChange = SelectPuzzleSize
                    , selected = Just model.puzzleSize
                    , label = Input.labelAbove [] (text "Puzzle Size")
                    , options =
                        [ Input.option Small (text "5x5")
                        , Input.option Medium (text "10x10")
                        , Input.option Large (text "15x15")
                        ]
                    }
                , Input.button [ centerX, centerY ]
                    { onPress = Just GenerateRandomGame
                    , label = text "Start"
                    }
                ]

        Won ->
            column
                [ centerX
                , centerY
                , width <| px 500
                , height <| px 500
                , Background.color (rgba 255 255 255 0.8)
                , Font.size 24
                ]
                [ el [ centerX, centerY ] (text "wow good job")
                , Input.button [ centerX, centerY ]
                    { onPress = Just ReturnToMainMenu
                    , label = text "Main Menu"
                    }
                ]


gameBoardRow : Int -> List CellState -> Element Msg
gameBoardRow y =
    let
        topBorderWidth =
            if y > 0 then
                2

            else
                0

        borderColor =
            if modBy 5 y == 0 then
                rgb255 0 0 0

            else
                rgb255 255 255 255
    in
    List.indexedMap (gameBoardCell y)
        >> row
            [ width fill
            , height fill
            , centerX
            , Border.color borderColor
            , Border.widthEach
                { bottom = 0
                , left = 0
                , right = 0
                , top = topBorderWidth
                }
            ]


gameBoardCell : Int -> Int -> CellState -> Element Msg
gameBoardCell y x cellState =
    let
        leftBorderWidth =
            if x > 0 then
                2

            else
                0

        borderColor =
            if modBy 5 x == 0 then
                rgb255 0 0 0

            else
                rgb255 255 255 255
    in
    el
        [ Background.color <| cellColor cellState
        , width fill
        , height fill
        , Events.onClick <| ToggleCell x y
        , Border.color borderColor
        , Border.widthEach
            { bottom = 0
            , left = leftBorderWidth
            , right = 0
            , top = 0
            }
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


rowHints : List (List Hint) -> Element Msg
rowHints hints =
    hints
        |> List.map (List.map hintElement)
        |> List.map (List.map <| el [ alignRight ])
        |> List.map (row [ height fill, spacing 8 ])
        |> column [ height <| px 500, spacing 4, paddingXY 20 0 ]


columnHints : List (List Hint) -> Element Msg
columnHints hints =
    hints
        |> List.map (List.map hintElement)
        |> List.map (List.map <| el [ centerX ])
        |> List.map (column [ width fill, spacing 8 ])
        |> row [ width <| px 500, spacing 4, centerX, paddingXY 0 20 ]


hintElement : Hint -> Element Msg
hintElement hint =
    let
        color =
            if hint.used then
                rgb255 100 100 100

            else
                rgb255 0 0 0
    in
    String.fromInt hint.groupSize
        |> text
        |> el [ Font.color color ]
