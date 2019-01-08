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
import Puzzle
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
    , puzzleSize : Puzzle.PuzzleSize
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
    , rowHints = []
    , columnHints = []
    , puzzleSize = Puzzle.Small
    , shiftPressed = False
    , gameState = Setup
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
    | SelectPuzzleSize Puzzle.PuzzleSize
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
            case model.gameState of
                Playing ->
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

                _ ->
                    ( model, Cmd.none )

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
    Sub.batch
        [ onKeyDown <| keyDecoder KeyDown
        , onKeyUp <| keyDecoder KeyUp
        ]


keyDecoder : (String -> Msg) -> Decoder Msg
keyDecoder msgConstructor =
    Decode.map msgConstructor (Decode.field "key" Decode.string)



-- VIEW


view : Model -> Html Msg
view model =
    gameView model
        |> layout
            [ Background.color darkGray
            , Font.color brightGreen
            , Font.family
                [ Font.typeface "Courier"
                , Font.monospace
                ]
            , Font.size 12
            ]


gameView : Model -> Element Msg
gameView model =
    row [ width <| px 800, height fill, centerX, onRight <| statusMessage model ] [ menu model, gameBoard model ]


gameBoard : Model -> Element Msg
gameBoard model =
    row [ centerX ]
        [ rowHints model.rowHints
        , column []
            [ columnHints model.columnHints
            , column
                [ centerX
                , centerY
                , width <| px 450
                , height <| px 450
                , Border.rounded 5
                , clip
                ]
                (model.board
                    |> Matrix.rows
                    |> List.map Array.toList
                    |> List.indexedMap gameBoardRow
                )
            ]
        ]


statusMessage : Model -> Element Msg
statusMessage model =
    case model.gameState of
        Won ->
            column [ centerY ]
                [ text "お"
                , text "め"
                , text "で"
                , text "と"
                , text "う"
                , text "！"
                ]
                |> el
                    [ height fill, Font.size 20 ]

        _ ->
            none


menu : Model -> Element Msg
menu model =
    column
        [ Font.size 16
        , spacing 20
        , width <| px 200
        , height <| px 500
        , centerY
        ]
        [ el [ centerX, alignTop, Font.size 20 ] (text "picross")
        , el [ centerX, centerY ]
            (Input.radio
                [ spacing 10, padding 20 ]
                { onChange = SelectPuzzleSize
                , selected = Just model.puzzleSize
                , label = Input.labelAbove [ centerX ] (text "select puzzle size")
                , options =
                    [ puzzleOption Puzzle.Small
                    , puzzleOption Puzzle.Medium
                    , puzzleOption Puzzle.Large
                    ]
                }
            )
        , Input.button [ centerX ]
            { onPress = Just ClearBoard
            , label = text "Clear Board"
            }
        , Input.button [ centerX ]
            { onPress = Just GenerateRandomGame
            , label = text "Start"
            }
        ]


puzzleOption : Puzzle.PuzzleSize -> Input.Option Puzzle.PuzzleSize Msg
puzzleOption puzzleSize =
    let
        label =
            case puzzleSize of
                Puzzle.Small ->
                    "5x5"

                Puzzle.Medium ->
                    "10x10"

                Puzzle.Large ->
                    "15x15"
    in
    Input.optionWith puzzleSize
        (\state ->
            let
                stateStyle =
                    case state of
                        Input.Idle ->
                            [ Border.color darkGray ]

                        Input.Focused ->
                            [ Border.color brightGreen ]

                        Input.Selected ->
                            [ Border.color brightGreen ]
            in
            el
                (stateStyle
                    ++ [ Border.width 1
                       , centerX
                       , padding 10
                       , Border.rounded 5
                       ]
                )
                (text label)
        )


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
                blue

            else
                darkGray

        borderStyle =
            if modBy 5 y == 0 then
                Border.dotted

            else
                Border.solid
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
            , borderStyle
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
                blue

            else
                darkGray

        borderStyle =
            if modBy 5 x == 0 then
                Border.dotted

            else
                Border.solid
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
        , borderStyle
        ]
        none


cellColor : CellState -> Color
cellColor cellState =
    case cellState of
        Empty ->
            dimGreen

        Filled ->
            brightGreen

        Flagged ->
            rgb255 255 0 0


rowHints : List (List Hint) -> Element Msg
rowHints hints =
    hints
        |> List.map (List.map hintElement)
        |> List.map (List.map <| el [ alignRight ])
        |> List.map (row [ height fill, spacing 8, alignRight ])
        |> column [ height <| px 450, spacing 4, paddingXY 10 0, alignBottom ]


columnHints : List (List Hint) -> Element Msg
columnHints hints =
    hints
        |> List.map (List.map hintElement)
        |> List.map (List.map <| el [ centerX ])
        |> List.map (column [ width fill, spacing 8, alignBottom ])
        |> row [ width <| px 450, spacing 4, centerX, paddingXY 0 10 ]


hintElement : Hint -> Element Msg
hintElement hint =
    let
        color =
            if hint.used then
                dimGreen

            else
                brightGreen
    in
    String.fromInt hint.groupSize
        |> text
        |> el [ Font.color color ]



-- COLORS


darkGray : Color
darkGray =
    rgb255 38 38 38


lightGray : Color
lightGray =
    rgb255 188 188 188


brightGreen : Color
brightGreen =
    rgba255 181 189 104 1


dimGreen : Color
dimGreen =
    rgba255 181 189 104 0.1


blue : Color
blue =
    rgba255 129 162 178 0.5
