-- src/Main.elm


module Main exposing (..)

import Browser
import DivisibilityRules
import Html exposing (Html, a, div, h1, h2, input, p, text)
import Html.Attributes exposing (href, placeholder, style, target, type_, value)
import Html.Events exposing (onInput)



-- This is what state our web app has/has to deal with.


type alias State =
    { number : String
    , numberDivisors : List Int
    }



-- Starting state


init : State
init =
    { number = ""
    , numberDivisors = []
    }



-- These are messages that the HTML may emit


type Msg
    = UpdateNumber String



-- Re-evaluate divisors using mathematical rules


update : Msg -> State -> State
update msg state =
    case msg of
        UpdateNumber newNumberString ->
            let
                -- Only process if it's a valid number (digits only)
                isValidNumber =
                    String.all Char.isDigit newNumberString || String.isEmpty newNumberString

                divisors =
                    if isValidNumber then
                        calculateDivisorsWithRules newNumberString

                    else
                        []

                -- No divisors for invalid input
            in
            { state
                | number = newNumberString
                , numberDivisors = divisors
            }



-- Main function that checks all divisibility rules 1-20


calculateDivisorsWithRules : String -> List Int
calculateDivisorsWithRules numberString =
    if String.isEmpty numberString then
        []

    else
        List.range 1 20
            |> List.filter (\divisor -> isDivisibleByRule divisor numberString)



-- Router function that calls the appropriate divisibility rule


isDivisibleByRule : Int -> String -> Bool
isDivisibleByRule divisor numberString =
    case divisor of
        1 ->
            DivisibilityRules.byOne numberString

        2 ->
            DivisibilityRules.byTwo numberString

        3 ->
            DivisibilityRules.byThree numberString

        4 ->
            DivisibilityRules.byFour numberString

        5 ->
            DivisibilityRules.byFive numberString

        6 ->
            DivisibilityRules.bySix numberString

        7 ->
            DivisibilityRules.bySeven numberString

        8 ->
            DivisibilityRules.byEight numberString

        9 ->
            DivisibilityRules.byNine numberString

        10 ->
            DivisibilityRules.byTen numberString

        11 ->
            DivisibilityRules.byEleven numberString

        12 ->
            DivisibilityRules.byTwelve numberString

        13 ->
            DivisibilityRules.byThirteen numberString

        14 ->
            DivisibilityRules.byFourteen numberString

        15 ->
            DivisibilityRules.byFifteen numberString

        16 ->
            DivisibilityRules.bySixteen numberString

        17 ->
            DivisibilityRules.bySeventeen numberString

        18 ->
            DivisibilityRules.byEighteen numberString

        19 ->
            DivisibilityRules.byNineteen numberString

        20 ->
            DivisibilityRules.byTwenty numberString

        _ ->
            False



-- Get the mathematical rule description for a divisor (plain text for now)


getRuleDescription : Int -> String
getRuleDescription divisor =
    case divisor of
        1 ->
            "n ≡ 0 (mod 1) for all n ∈ ℤ"

        2 ->
            "n ≡ 0 (mod 2) ⟺ last digit ∈ {0,2,4,6,8}"

        3 ->
            "n ≡ 0 (mod 3) ⟺ Σ(digits) ≡ 0 (mod 3)"

        4 ->
            "n ≡ 0 (mod 4) ⟺ n even ∧ last two digits ≡ 0 (mod 4)"

        5 ->
            "n ≡ 0 (mod 5) ⟺ last digit ∈ {0,5}"

        6 ->
            "n ≡ 0 (mod 6) ⟺ n ≡ 0 (mod 2) ∧ n ≡ 0 (mod 3)"

        7 ->
            "n ≡ 0 (mod 7) ⟺ (n' - 2d₀) ≡ 0 (mod 7) where n = 10n' + d₀"

        8 ->
            "n ≡ 0 (mod 8) ⟺ last three digits ≡ 0 (mod 8)"

        9 ->
            "n ≡ 0 (mod 9) ⟺ Σ(digits) ≡ 0 (mod 9)"

        10 ->
            "n ≡ 0 (mod 10) ⟺ last digit = 0"

        11 ->
            "n ≡ 0 (mod 11) ⟺ Σ((-1)ⁱ × dᵢ) ≡ 0 (mod 11)"

        12 ->
            "n ≡ 0 (mod 12) ⟺ n ≡ 0 (mod 3) ∧ n ≡ 0 (mod 4)"

        13 ->
            "n ≡ 0 (mod 13) ⟺ (n' + 4d₀) ≡ 0 (mod 13) where n = 10n' + d₀"

        14 ->
            "n ≡ 0 (mod 14) ⟺ n ≡ 0 (mod 2) ∧ n ≡ 0 (mod 7)"

        15 ->
            "n ≡ 0 (mod 15) ⟺ n ≡ 0 (mod 3) ∧ n ≡ 0 (mod 5)"

        16 ->
            "n ≡ 0 (mod 16) ⟺ last four digits ≡ 0 (mod 16)"

        17 ->
            "n ≡ 0 (mod 17) ⟺ (n' + 5d₀) ≡ 0 (mod 17) where n = 10n' + d₀"

        18 ->
            "n ≡ 0 (mod 18) ⟺ n ≡ 0 (mod 2) ∧ n ≡ 0 (mod 9)"

        19 ->
            "n ≡ 0 (mod 19) ⟺ (n' + 2d₀) ≡ 0 (mod 19) where n = 10n' + d₀"

        20 ->
            "n ≡ 0 (mod 20) ⟺ last two digits ∈ {00,20,40,60,80}"

        _ ->
            ""


view : State -> Html Msg
view state =
    div
        [ style "max-width" "900px"
        , style "margin" "0 auto"
        , style "padding" "40px 20px"
        , style "font-family" "-apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif"
        , style "line-height" "1.6"
        , style "color" "#2c3e50"
        ]
        [ -- Header
          div [ style "margin-bottom" "40px" ]
            [ h1
                [ style "font-size" "28px"
                , style "font-weight" "600"
                , style "margin" "0 0 8px 0"
                , style "color" "#1a252f"
                ]
                [ text "Integer Divisibility Analysis" ]
            , p
                [ style "font-size" "16px"
                , style "color" "#6c757d"
                , style "margin" "0"
                ]
                [ text "Computational verification of divisibility using mathematical rules" ]
            ]

        -- Input section
        , div [ style "margin-bottom" "32px" ]
            [ input
                [ type_ "text"
                , placeholder "Enter an integer"
                , value state.number
                , onInput UpdateNumber
                , style "width" "100%"
                , style "padding" "12px 16px"
                , style "font-size" "16px"
                , style "border" "1px solid #dee2e6"
                , style "border-radius" "4px"
                , style "box-sizing" "border-box"
                , style "font-family" "inherit"
                ]
                []
            ]

        -- Results section
        , if not (String.isEmpty state.number) then
            div []
                [ -- Divisors
                  div [ style "margin-bottom" "32px" ]
                    [ h2
                        [ style "font-size" "18px"
                        , style "font-weight" "600"
                        , style "margin" "0 0 12px 0"
                        , style "color" "#1a252f"
                        ]
                        [ text "Divisors" ]
                    , div
                        [ style "font-size" "24px"
                        , style "color" "#2563eb"
                        , style "font-weight" "500"
                        , style "margin" "8px 0"
                        ]
                        [ text (formatDivisorsAsSet state.numberDivisors) ]
                    ]

                -- Mathematical rules used
                , if not (List.isEmpty state.numberDivisors) then
                    div [ style "margin-bottom" "40px" ]
                        [ h2
                            [ style "font-size" "18px"
                            , style "font-weight" "600"
                            , style "margin" "0 0 16px 0"
                            , style "color" "#1a252f"
                            ]
                            [ text "Mathematical Rules Applied" ]
                        , div
                            [ style "background-color" "#f8f9fa"
                            , style "border" "1px solid #e9ecef"
                            , style "border-radius" "4px"
                            , style "padding" "20px"
                            ]
                            [ div [] (List.map renderRule state.numberDivisors) ]
                        ]

                  else
                    text ""
                ]

          else
            text ""

        -- Commentary section
        , div
            [ style "margin-top" "48px"
            , style "padding-top" "32px"
            , style "border-top" "1px solid #e9ecef"
            ]
            [ h2
                [ style "font-size" "18px"
                , style "font-weight" "600"
                , style "margin" "0 0 16px 0"
                , style "color" "#1a252f"
                ]
                [ text "On Computational Complexity" ]
            , div
                [ style "font-size" "15px"
                , style "line-height" "1.7"
                , style "color" "#495057"
                ]
                [ p [ style "margin" "0 0 16px 0" ]
                    [ text "Fair warning: your computer will crash if you type in a number that's too large since figuring out the set of numbers that all divide a particular number (written as {x ∈ ℤ : n ≡ 0 (mod x)}) is genuinely hard." ]
                , p [ style "margin" "0 0 16px 0" ]
                    [ text "It's not feasible to keep adding divisibility rules for numbers ad-infinitum since numbers go on infinitely. So the standard approach for finding all divisors is prime factorization. But with really large numbers, figuring out their factors becomes a "
                    , Html.em [] [ text "seriously" ]
                    , text " difficult, computationally infeasible problem (at least currently with classical computers)."
                    ]
                , p [ style "margin" "0 0 16px 0" ]
                    [ text "What you're seeing crash with these simple divisibility rules is just the surface level. The deeper issue—factoring the massive numbers used in RSA encryption—is what keeps internet security functioning. Banks, governments, and secure websites rely on this computational hardness remaining unsolved." ]
                ]
            ]

        -- Footer
        , div
            [ style "margin-top" "64px"
            , style "padding-top" "24px"
            , style "border-top" "1px solid #e9ecef"
            , style "text-align" "center"
            , style "font-size" "14px"
            , style "color" "#6c757d"
            ]
            [ p [ style "margin" "0" ]
                [ text "Built with "
                , a
                    [ href "https://elm-lang.org"
                    , target "_blank"
                    , style "color" "#2563eb"
                    , style "text-decoration" "none"
                    ]
                    [ text "Elm" ]
                , text ", a purely functional programming language "
                ]
            ]
        ]



-- Render a mathematical rule


renderRule : Int -> Html Msg
renderRule divisor =
    div
        [ style "margin-bottom" "12px"
        , style "font-size" "14px"
        ]
        [ div
            [ style "font-weight" "600"
            , style "color" "#1a252f"
            , style "margin-bottom" "4px"
            ]
            [ text ("Rule " ++ String.fromInt divisor ++ ":") ]
        , div
            [ style "color" "#495057"
            , style "font-family" "Consolas, 'Courier New', monospace"
            ]
            [ text (getRuleDescription divisor) ]
        ]



-- Helper function to format divisors as a mathematical set


formatDivisorsAsSet : List Int -> String
formatDivisorsAsSet divisors =
    if List.isEmpty divisors then
        "∅"

    else
        "{ " ++ String.join ", " (List.map String.fromInt divisors) ++ " }"



-- MAIN (wire everything together)


main : Program () State Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
