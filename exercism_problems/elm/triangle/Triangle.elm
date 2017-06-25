module Triangle exposing (triangleKind, Triangle(..))

import Set


version : Int
version =
    2


type Triangle
    = Equilateral
    | Isosceles
    | Scalene


checkValidity : List Float -> Result String (List Float)
checkValidity sides =
    case (List.filter (\a -> a <= 0) sides) of
        [] ->
            Ok (List.sort sides)

        _ ->
            Err "Invalid lengths"


checkInequality : List Float -> Result String (List Float)
checkInequality sides =
    case sides of
        [ a, b, c ] ->
            if a + b > c then
                Ok sides
            else
                Err "Violates inequality"

        _ ->
            Err "Violates inequality"


findTriangleType : List Float -> Result String Triangle
findTriangleType sides =
    case (Set.fromList sides |> Set.size) of
        1 ->
            Ok Equilateral

        2 ->
            Ok Isosceles

        _ ->
            Ok Scalene


triangleKind : Float -> Float -> Float -> Result String Triangle
triangleKind a b c =
    let
        sides =
            a :: b :: c :: []
    in
        checkValidity sides |> Result.andThen checkInequality |> Result.andThen findTriangleType
