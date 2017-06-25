module Raindrops exposing (..)

initializeList: Int -> List Int
initializeList n = List.range 2 n

isNotDivisible: Int -> Int -> Bool
isNotDivisible x y = rem y x /= 0

isDivisible: Int -> Int -> Bool
isDivisible x y = rem x y == 0

findAllPrimes: List Int -> List Int
findAllPrimes sieve = 
    case sieve of
        (x :: xs) -> x :: findAllPrimes (List.filter (isNotDivisible x) xs)
        _ -> []

findPrimeFactors: Int -> List Int -> List Int
findPrimeFactors n primes = 
    case List.filter (isDivisible n) primes of
        [] -> [n]
        x -> x

rainDrop: Int -> Maybe String
rainDrop factor = 
    case factor of
        3 -> Just "Pling"
        5 -> Just "Plang"
        7 -> Just "Plong"
        _ -> Nothing

concatDrops: Maybe String -> String -> String
concatDrops drop soFar = 
    case drop of
        Just x -> soFar ++ x
        Nothing -> soFar

result: Int -> List (Maybe String) -> String
result num drops = 
    case List.foldl concatDrops "" drops of
        "" -> toString num
        x -> x

raindrops: Int -> String
raindrops num =
    let
        factors = (findPrimeFactors num) <| findAllPrimes <| initializeList num
    in
        result num (List.map rainDrop factors)