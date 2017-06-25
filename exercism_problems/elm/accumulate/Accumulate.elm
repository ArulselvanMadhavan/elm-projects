module Accumulate exposing (..)


accumulate : (a -> a) -> List a -> List a
accumulate f lst =
    case lst of
        x :: xs ->
            (f x) :: (accumulate f xs)

        [] ->
            []
