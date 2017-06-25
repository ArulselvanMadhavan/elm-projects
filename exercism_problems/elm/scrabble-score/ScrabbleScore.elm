module ScrabbleScore exposing (..)

import Dict exposing (Dict)
import String exposing (toUpper)


onePoints =
    ( 1, [ 'A', 'E', 'I', 'O', 'U', 'L', 'N', 'R', 'S', 'T' ] )


twoPoints =
    ( 2, [ 'D', 'G' ] )


threePoints =
    ( 3, [ 'B', 'C', 'M', 'P' ] )


fourPoints =
    ( 4, [ 'F', 'H', 'V', 'W', 'Y' ] )


fivePoints =
    ( 5, [ 'K' ] )


eightPoints =
    ( 8, [ 'J', 'X' ] )


tenPoints =
    ( 10, [ 'Q', 'Z' ] )


scoreSheet =
    [ onePoints, twoPoints, threePoints, fourPoints, fivePoints, eightPoints, tenPoints ]


assignScores : ( Int, List Char ) -> List ( Char, Int )
assignScores ( pt, lst ) =
    List.map (\x -> ( x, pt )) lst


getScore : Dict Char Int -> Char -> Int
getScore scoreSheet c =
    Maybe.withDefault 0 (Dict.get c scoreSheet)


getScores : Dict Char Int -> List Char -> List Int
getScores scoreSheet chars =
    List.map (getScore scoreSheet) chars


scoreWord : String -> Int
scoreWord str =
    let
        scores =
            List.concatMap assignScores scoreSheet
                |> Dict.fromList
    in
        toUpper str
            |> String.toList
            |> getScores scores
            |> List.sum
