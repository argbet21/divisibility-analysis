-- src/DivisibilityRules.elm


module DivisibilityRules exposing (..)

import Helpers exposing (..)



-- Rule 1: Everything is divisible by 1


byOne : String -> Bool
byOne numberString =
    not (String.isEmpty numberString)



-- Rule 2: Last digit is even (0,2,4,6,8)


byTwo : String -> Bool
byTwo numberString =
    let
        lastDigit =
            getLastDigits 1 numberString |> stringToInt
    in
    modBy 2 lastDigit == 0



-- Rule 3: Sum of digits must be a multiple of 3


byThree : String -> Bool
byThree numberString =
    let
        digitSum =
            sumOfDigits numberString
    in
    if digitSum < 10 then
        digitSum == 3 || digitSum == 6 || digitSum == 9

    else
        -- Recursively apply rule to the sum
        byThree (String.fromInt digitSum)



-- Rule 4: Number is even AND last 2 digits are a multiple of 4


byFour : String -> Bool
byFour numberString =
    let
        isEven =
            byTwo numberString

        lastTwoDigits =
            getLastDigits 2 numberString

        lastTwoValid =
            isStringDivisibleBy 4 lastTwoDigits
    in
    isEven && lastTwoValid



-- Rule 5: Last digit must be 5 or 0


byFive : String -> Bool
byFive numberString =
    let
        lastDigit =
            getLastDigits 1 numberString
    in
    lastDigit == "5" || lastDigit == "0"



-- Rule 6: Number is even AND divisible by 3


bySix : String -> Bool
bySix numberString =
    byTwo numberString && byThree numberString



-- Rule 7: Double the last digit, subtract from remaining digits, result should be a multiple of 7


bySeven : String -> Bool
bySeven numberString =
    if String.length numberString <= 1 then
        let
            digit =
                stringToInt numberString
        in
        digit == 0 || digit == 7

    else
        let
            lastDigit =
                getLastDigits 1 numberString |> stringToInt

            remainingDigits =
                getAllButLastDigits 1 numberString |> stringToInt

            result =
                remainingDigits - (2 * lastDigit)
        in
        if result < 0 then
            bySeven (String.fromInt -result)

        else
            bySeven (String.fromInt result)



-- Rule 8: Last 3 digits should be a multiple of 8


byEight : String -> Bool
byEight numberString =
    let
        lastThreeDigits =
            getLastDigits 3 numberString
    in
    isStringDivisibleBy 8 lastThreeDigits



-- Rule 9: Add up digits recursively until single digit, must be 9


byNine : String -> Bool
byNine numberString =
    let
        digitSum =
            sumOfDigits numberString
    in
    if digitSum < 10 then
        digitSum == 9

    else
        -- Recursively apply rule to the sum
        byNine (String.fromInt digitSum)



-- Rule 10: Last digit must be 0


byTen : String -> Bool
byTen numberString =
    getLastDigits 1 numberString == "0"



-- Rule 11: Alternating sum of digits (from right) must be divisible by 11


byEleven : String -> Bool
byEleven numberString =
    let
        digits =
            numberString |> String.toList |> List.map charToInt |> List.reverse

        alternatingSum =
            calculateAlternatingSum digits 1
    in
    modBy 11 (abs alternatingSum) == 0



-- Helper for rule 11: Calculate alternating sum


calculateAlternatingSum : List Int -> Int -> Int
calculateAlternatingSum digits multiplier =
    case digits of
        [] ->
            0

        head :: tail ->
            (head * multiplier) + calculateAlternatingSum tail -multiplier



-- Rule 12: Must be divisible by both 3 and 4


byTwelve : String -> Bool
byTwelve numberString =
    byThree numberString && byFour numberString



-- Rule 13: Multiply last digit by 4, add to remaining digits, result must be a multiple of 13


byThirteen : String -> Bool
byThirteen numberString =
    if String.length numberString <= 1 then
        let
            digit =
                stringToInt numberString
        in
        digit == 0 || modBy 13 digit == 0

    else
        let
            lastDigit =
                getLastDigits 1 numberString |> stringToInt

            remainingDigits =
                getAllButLastDigits 1 numberString |> stringToInt

            result =
                remainingDigits + (4 * lastDigit)
        in
        byThirteen (String.fromInt result)



-- Rule 14: Apply rules for 2 and 7


byFourteen : String -> Bool
byFourteen numberString =
    byTwo numberString && bySeven numberString



-- Rule 15: Apply rules for 3 and 5


byFifteen : String -> Bool
byFifteen numberString =
    byThree numberString && byFive numberString



-- Rule 16: Last 4 digits must be divisible by 16


bySixteen : String -> Bool
bySixteen numberString =
    let
        lastFourDigits =
            getLastDigits 4 numberString
    in
    isStringDivisibleBy 16 lastFourDigits



-- Rule 17: Multiply last digit by 5, add to remaining digits, result must be a multiple of 17


bySeventeen : String -> Bool
bySeventeen numberString =
    if String.length numberString <= 1 then
        let
            digit =
                stringToInt numberString
        in
        digit == 0 || modBy 17 digit == 0

    else
        let
            lastDigit =
                getLastDigits 1 numberString |> stringToInt

            remainingDigits =
                getAllButLastDigits 1 numberString |> stringToInt

            result =
                remainingDigits + (5 * lastDigit)
        in
        bySeventeen (String.fromInt result)



-- Rule 18: Apply rules for 2 and 9


byEighteen : String -> Bool
byEighteen numberString =
    byTwo numberString && byNine numberString



-- Rule 19: Multiply last digit by 2, add to remaining digits, result must be a multiple of 19


byNineteen : String -> Bool
byNineteen numberString =
    if String.length numberString <= 1 then
        let
            digit =
                stringToInt numberString
        in
        digit == 0 || modBy 19 digit == 0

    else
        let
            lastDigit =
                getLastDigits 1 numberString |> stringToInt

            remainingDigits =
                getAllButLastDigits 1 numberString |> stringToInt

            result =
                remainingDigits + (2 * lastDigit)
        in
        byNineteen (String.fromInt result)



-- Rule 20: Must end with 00, 20, 40, 60, or 80


byTwenty : String -> Bool
byTwenty numberString =
    let
        lastTwoDigits =
            getLastDigits 2 numberString
    in
    lastTwoDigits == "00" || lastTwoDigits == "20" || lastTwoDigits == "40" || lastTwoDigits == "60" || lastTwoDigits == "80"
