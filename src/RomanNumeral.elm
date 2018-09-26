module RomanNumeral exposing (arabicToRoman)

{-| Convert an Arabic numeral to a Roman one.

@docs arabicToRoman

-}


{-|

    arabicToRoman 123 == Just "CXXIII"

    arabicToRoman 0 == Nothing

-}
arabicToRoman : Int -> Maybe String
arabicToRoman arabic =
    let
        digits =
            String.fromInt arabic
                |> String.toList
                |> List.reverse
                |> List.map String.fromChar
                |> List.map (\char -> String.toInt char |> Maybe.withDefault 0)

        parts =
            List.reverse <|
                List.indexedMap (\exp -> \digit -> digit * 10 ^ exp) digits
    in
    if arabic > 0 && arabic < 4000 then
        Just <| String.concat <| List.map reduce parts

    else
        Nothing


reduce : Int -> String
reduce arabic =
    let
        exponent =
            logBase10 arabic

        magnitude =
            10 ^ exponent

        char1 =
            convert magnitude

        char5 =
            convert (5 * magnitude)

        char10 =
            convert (10 * magnitude)

        msd =
            arabic // magnitude
    in
    case msd of
        0 ->
            ""

        1 ->
            char1

        2 ->
            char1 ++ char1

        3 ->
            char1 ++ char1 ++ char1

        4 ->
            char1 ++ char5

        5 ->
            char5

        6 ->
            char5 ++ char1

        7 ->
            char5 ++ char1 ++ char1

        8 ->
            char5 ++ char1 ++ char1 ++ char1

        9 ->
            char1 ++ char10

        10 ->
            char10

        _ ->
            ""


convert : Int -> String
convert arabic =
    case arabic of
        1 ->
            "I"

        5 ->
            "V"

        10 ->
            "X"

        50 ->
            "L"

        100 ->
            "C"

        500 ->
            "D"

        1000 ->
            "M"

        _ ->
            ""


logBase10 : Int -> Int
logBase10 int =
    logBase 10 (toFloat int) |> floor
