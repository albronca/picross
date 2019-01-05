module Puzzle exposing (Puzzle, allPuzzles, empty, height, width)

import Array exposing (Array)
import Matrix exposing (Matrix)


type alias Puzzle =
    { id : Int
    , name : String
    , solution : Matrix Bool
    }


type alias PuzzleSeed =
    { id : Int
    , name : String
    , solution : List (List Int)
    }


allPuzzles : Array Puzzle
allPuzzles =
    [ { id = 1
      , name = "left arrow"
      , solution =
            [ [ 0, 0, 1, 0, 0 ]
            , [ 0, 1, 1, 0, 0 ]
            , [ 1, 1, 1, 1, 1 ]
            , [ 0, 1, 1, 0, 0 ]
            , [ 0, 0, 1, 0, 0 ]
            ]
      }
    , { id = 2
      , name = "division sign"
      , solution =
            [ [ 0, 0, 1, 0, 0 ]
            , [ 0, 0, 0, 0, 0 ]
            , [ 1, 1, 1, 1, 1 ]
            , [ 0, 0, 0, 0, 0 ]
            , [ 0, 0, 1, 0, 0 ]
            ]
      }
    , { id = 3
      , name = "black small sqaure"
      , solution =
            [ [ 0, 0, 0, 0, 0 ]
            , [ 0, 1, 1, 1, 0 ]
            , [ 0, 1, 1, 1, 0 ]
            , [ 0, 1, 1, 1, 0 ]
            , [ 0, 0, 0, 0, 0 ]
            ]
      }
    , { id = 4
      , name = "question mark"
      , solution =
            [ [ 0, 1, 1, 1, 0 ]
            , [ 0, 0, 0, 1, 0 ]
            , [ 0, 0, 1, 0, 0 ]
            , [ 0, 0, 0, 0, 0 ]
            , [ 0, 0, 1, 0, 0 ]
            ]
      }
    , { id = 5
      , name = "double exclamation mark"
      , solution =
            [ [ 0, 1, 0, 1, 0 ]
            , [ 0, 1, 0, 1, 0 ]
            , [ 0, 1, 0, 1, 0 ]
            , [ 0, 0, 0, 0, 0 ]
            , [ 0, 1, 0, 1, 0 ]
            ]
      }
    , { id = 6
      , name = "roman cross"
      , solution =
            [ [ 0, 0, 0, 0, 0 ]
            , [ 0, 0, 1, 0, 0 ]
            , [ 0, 1, 1, 1, 0 ]
            , [ 0, 0, 1, 0, 0 ]
            , [ 0, 0, 1, 0, 0 ]
            ]
      }
    , { id = 7
      , name = "japanese 'here' button"
      , solution =
            [ [ 0, 0, 0, 0, 0 ]
            , [ 1, 1, 0, 1, 1 ]
            , [ 0, 1, 0, 0, 1 ]
            , [ 1, 1, 0, 1, 1 ]
            , [ 0, 0, 0, 0, 0 ]
            ]
      }
    , { id = 8
      , name = "black heart"
      , solution =
            [ [ 0, 1, 0, 1, 0 ]
            , [ 1, 1, 1, 1, 1 ]
            , [ 1, 1, 1, 1, 1 ]
            , [ 0, 1, 1, 1, 0 ]
            , [ 0, 0, 1, 0, 0 ]
            ]
      }
    , { id = 9
      , name = "eyes"
      , solution =
            [ [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
            , [ 1, 1, 1, 1, 0, 0, 1, 1, 1, 1 ]
            , [ 1, 0, 0, 1, 0, 0, 1, 0, 0, 1 ]
            , [ 1, 0, 0, 1, 0, 0, 1, 0, 0, 1 ]
            , [ 1, 0, 1, 1, 0, 0, 1, 0, 1, 1 ]
            , [ 1, 0, 1, 1, 0, 0, 1, 0, 1, 1 ]
            , [ 1, 0, 1, 1, 0, 0, 1, 0, 1, 1 ]
            , [ 1, 0, 1, 1, 0, 0, 1, 0, 1, 1 ]
            , [ 1, 1, 1, 1, 0, 0, 1, 1, 1, 1 ]
            , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
            ]
      }
    ]
        |> List.map fromPuzzleSeed
        |> Array.fromList


fromPuzzleSeed : PuzzleSeed -> Puzzle
fromPuzzleSeed { id, name, solution } =
    solution
        |> Matrix.fromList
        |> Maybe.withDefault Matrix.empty
        |> Matrix.map ((==) 1)
        |> Puzzle id name


height : Puzzle -> Int
height =
    .solution >> Matrix.height


width : Puzzle -> Int
width =
    .solution >> Matrix.width


empty : Puzzle
empty =
    { id = 0
    , name = ""
    , solution = Matrix.empty
    }
