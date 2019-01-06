module Puzzle exposing
    ( PuzzleSize(..)
    , solutionGenerator
    )

import Array exposing (Array)
import Matrix exposing (Matrix)
import Random
import Random.Extra


type alias PuzzleSeed =
    { id : Int
    , name : String
    , solution : List (List Int)
    }


type PuzzleSize
    = Small
    | Medium
    | Large


solutionGenerator : PuzzleSize -> Random.Generator (Matrix Bool)
solutionGenerator puzzleSize =
    let
        edgeLength =
            edgeLengthFor puzzleSize
    in
    Random.Extra.bool
        |> Random.list edgeLength
        |> Random.list edgeLength
        |> Random.map (Matrix.fromList >> Maybe.withDefault Matrix.empty)


edgeLengthFor : PuzzleSize -> Int
edgeLengthFor puzzleSize =
    case puzzleSize of
        Small ->
            5

        Medium ->
            10

        Large ->
            15
