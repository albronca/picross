module Matrix exposing
    ( Matrix
    , columns
    , empty
    , filter
    , fromList
    , get
    , getColumn
    , getRow
    , height
    , indexedMap
    , map
    , repeat
    , rows
    , set
    , toIndexedArray
    , update
    , width
    )

import Array exposing (Array)



{- A matrix implemention for Elm.
   Internally it uses a flat array for speed reasons.

   adapted from eeue56/elm-flat-matrix for 0.19
   <https://package.elm-lang.org/packages/eeue56/elm-flat-matrix/4.0.0>
-}


{-| Matrix a has a given size, and data contained within
-}
type alias Matrix a =
    { size : ( Int, Int )
    , data : Array a
    }


{-| Create an empty matrix
-}
empty : Matrix a
empty =
    { size = ( 0, 0 ), data = Array.empty }


{-| Width of a given matrix
-}
width : Matrix a -> Int
width matrix =
    Tuple.first matrix.size


{-| Height of a given matrix
-}
height : Matrix a -> Int
height matrix =
    Tuple.second matrix.size


{-| Create a matrix of a given size `x y` with a default value of `v`
-}
repeat : Int -> Int -> a -> Matrix a
repeat x y v =
    { size = ( x, y )
    , data = Array.repeat (x * y) v
    }


{-| Create a matrix from a list of lists.
If the lists within the list are not consistently sized, return `Nothing`
Otherwise return a matrix with the size as the size of the outer and nested lists.
The outer list represents the y axis and inner lists represent the x axis.
Eg:
[ [ {x=0, y=0}, {x=1, y=0}, {x=2, y=0} ]
, [ {x=0, y=1}, {x=1, y=1}, {x=2, y=1} ]
, [ {x=0, y=2}, {x=1, y=2}, {x=2, y=2} ]
]
-}
fromList : List (List a) -> Maybe (Matrix a)
fromList list =
    let
        -- the number of elements in the top level list is taken as height
        h =
            List.length list

        -- the number of elements in the first element is taken as the width
        w =
            List.length <|
                case List.head list of
                    Just x ->
                        x

                    Nothing ->
                        []

        -- ensure that all "rows" are the same size
        allSame =
            List.isEmpty <| List.filter (\x -> List.length x /= w) list
    in
    if not allSame then
        Nothing

    else
        Just { size = ( w, h ), data = Array.fromList <| List.concat list }


{-| Get a value from a given `x y` and return `Just v` if it exists
Otherwise `Nothing`
-}
get : Int -> Int -> Matrix a -> Maybe a
get i j matrix =
    let
        pos =
            (j * width matrix) + i
    in
    if (i < width matrix && i > -1) && (j < height matrix && j > -1) then
        Array.get pos matrix.data

    else
        Nothing


{-| Get a row at a given j
-}
getRow : Int -> Matrix a -> Maybe (Array a)
getRow j matrix =
    let
        start =
            j * width matrix

        end =
            start + width matrix
    in
    if end > (width matrix * height matrix) then
        Nothing

    else
        Just <| Array.slice start end matrix.data


{-| Get a column at a given i
-}
getColumn : Int -> Matrix a -> Maybe (Array a)
getColumn i matrix =
    let
        w =
            Tuple.first matrix.size

        h =
            Tuple.second matrix.size

        indices =
            List.map (\x -> x * w + i) (List.range 0 (h - 1))
    in
    if i >= w then
        Nothing

    else
        Just <|
            Array.fromList <|
                List.foldl
                    (\index ls ->
                        case Array.get index matrix.data of
                            Just v ->
                                ls ++ [ v ]

                            Nothing ->
                                ls
                    )
                    []
                    indices


{-| Set a value at a given `i, j` in the matrix and return the new matrix
If the `i, j` is out of bounds then return the unmodified matrix
-}
set : Int -> Int -> a -> Matrix a -> Matrix a
set i j v matrix =
    let
        pos =
            (j * Tuple.first matrix.size) + i
    in
    if (i < width matrix && i > -1) && (j < height matrix && j > -1) then
        { matrix | data = Array.set pos v matrix.data }

    else
        matrix


{-| Update an element at `x, y` with the given update function
If out of bounds, return the matrix unchanged
-}
update : Int -> Int -> (a -> a) -> Matrix a -> Matrix a
update x y f matrix =
    case get x y matrix of
        Nothing ->
            matrix

        Just v ->
            set x y (f v) matrix


{-| Apply a function of every element in the matrix
-}
map : (a -> b) -> Matrix a -> Matrix b
map f matrix =
    { size = matrix.size, data = Array.map f matrix.data }


{-| Apply a function, taking the `x, y` of every element in the matrix
-}
indexedMap : (Int -> Int -> a -> b) -> Matrix a -> Matrix b
indexedMap f matrix =
    let
        f_ i v =
            let
                x =
                    modBy (width matrix) i

                y =
                    i // width matrix
            in
            f x y v
    in
    { size = matrix.size, data = Array.fromList <| List.indexedMap f_ <| Array.toList matrix.data }


{-| Keep only elements that return `True` when passed to the given function f
-}
filter : (a -> Bool) -> Matrix a -> Array a
filter f matrix =
    Array.filter f matrix.data


{-| Convert a matrix to an indexed array
-}
toIndexedArray : Matrix a -> Array ( ( Int, Int ), a )
toIndexedArray matrix =
    (indexedMap (\x y v -> ( ( x, y ), v )) matrix).data


rows : Matrix a -> List (Array a)
rows matrix =
    List.range 0 (height matrix - 1)
        |> List.map (\y -> getRow y matrix)
        |> List.map (Maybe.withDefault Array.empty)


columns : Matrix a -> List (Array a)
columns matrix =
    List.range 0 (width matrix - 1)
        |> List.map (\x -> getColumn x matrix)
        |> List.map (Maybe.withDefault Array.empty)
