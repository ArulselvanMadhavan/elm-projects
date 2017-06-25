module Anagram exposing (..)


filterBasedOnLength : Int -> List String -> List String
filterBasedOnLength wordLength options =
    List.filter (\x -> String.length x == wordLength) options


sortWord : String -> String
sortWord str =
    String.toLower str
        |> String.toList
        |> List.sort
        |> String.fromList


filterAnagrams : String -> List String -> List String
filterAnagrams word options =
    let
        sortedWord =
            sortWord word
    in
        List.filter (\x -> (sortWord x) == sortedWord) options


filterSameWord : String -> List String -> List String
filterSameWord str options =
    List.filter (\x -> (String.toLower x) /= str) options


detect : String -> List String -> List String
detect str options =
    let
        word =
            String.toLower str
    in
        filterBasedOnLength (String.length word) options
            |> filterSameWord word
            |> Debug.log "After Filtering"
            |> filterAnagrams word
