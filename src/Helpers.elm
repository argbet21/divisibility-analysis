-- src/Helpers.elm


module Helpers exposing (..)

-- Get the last N digits of a number string


getLastDigits : Int -> String -> String
getLastDigits n numberString =
    let
        length =
            String.length numberString

        startIndex =
            max 0 (length - n)
    in
    String.dropLeft startIndex numberString



-- Get all but the last N digits


getAllButLastDigits : Int -> String -> String
getAllButLastDigits n numberString =
    let
        length =
            String.length numberString

        keepLength =
            max 0 (length - n)
    in
    String.left keepLength numberString



-- Calculate sum of all digits in a string


sumOfDigits : String -> Int
sumOfDigits numberString =
    numberString
        |> String.toList
        |> List.map charToInt
        |> List.sum



-- Convert single character to integer (returns 0 for non-digits)


charToInt : Char -> Int
charToInt char =
    case String.toInt (String.fromChar char) of
        Just n ->
            n

        Nothing ->
            0



-- Check if a string represents a number divisible by n using modBy
-- (Used for recursive calls in complex rules)


isStringDivisibleBy : Int -> String -> Bool
isStringDivisibleBy divisor numberString =
    case String.toInt numberString of
        Just num ->
            modBy divisor num == 0

        Nothing ->
            False



-- Convert string to int, return 0 if invalid


stringToInt : String -> Int
stringToInt str =
    String.toInt str |> Maybe.withDefault 0
