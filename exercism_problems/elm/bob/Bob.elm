module Bob exposing (..)
import Char

isAnAlphabet: Char -> Bool
isAnAlphabet char = 
    Char.isUpper char || Char.isLower char

filterAlphabets: List Char -> List Char
filterAlphabets chars =
    List.filter isAnAlphabet chars

isShouting: String -> Bool
isShouting message = 
    let
        chars = filterAlphabets (String.toList message)
    in
        not (List.isEmpty chars) && List.all Char.isUpper chars

isQuestion: String -> Bool
isQuestion message = 
    String.endsWith "?" message

isWhitespace: String -> Bool
isWhitespace message =
    String.isEmpty (String.trim message)

hey: String -> String
hey message = 
    if isWhitespace message then
        "Fine. Be that way!"
    else if isShouting message then
        "Whoa, chill out!"
    else if isQuestion message then
        "Sure."
    else
        "Whatever."