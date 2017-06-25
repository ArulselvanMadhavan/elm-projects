module Pangram exposing (..)

import Char
import Array


hasValidLength : String -> Bool
hasValidLength str =
    if String.length str < 26 then
        False
    else
        True


isValidChar : Char -> Array.Array Bool -> Array.Array Bool
isValidChar c soFar =
    if Char.toCode c <= 122 && Char.toCode c >= 97 then
        Array.set (Char.toCode c - 97) True soFar
    else
        Array.set (Char.toCode c - 97) False soFar


checkCharsForPangram : String -> Bool
checkCharsForPangram str =
    String.toList str
        |> List.map Char.toLower
        |> List.foldl isValidChar (Array.initialize 26 (always False))
        |> Array.foldl (&&) True


isPangram : String -> Bool
isPangram str =
    if hasValidLength str then
        checkCharsForPangram str
    else
        False
