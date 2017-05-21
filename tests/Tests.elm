module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz
import String
import RomanNumeral


all : Test
all =
    describe "RomanNumeral.arabicToRoman"
        [ describe "test valid numbers"
            [ test "1" <| \() -> assert 1 "I"
            , test "2" <| \() -> assert 2 "II"
            , test "3" <| \() -> assert 3 "III"
            , test "4" <| \() -> assert 4 "IV"
            , test "5" <| \() -> assert 5 "V"
            , test "6" <| \() -> assert 6 "VI"
            , test "7" <| \() -> assert 7 "VII"
            , test "8" <| \() -> assert 8 "VIII"
            , test "9" <| \() -> assert 9 "IX"
            , test "10" <| \() -> assert 10 "X"
            , test "11" <| \() -> assert 11 "XI"
            , test "12" <| \() -> assert 12 "XII"
            , test "14" <| \() -> assert 14 "XIV"
            , test "15" <| \() -> assert 15 "XV"
            , test "16" <| \() -> assert 16 "XVI"
            , test "17" <| \() -> assert 17 "XVII"
            , test "19" <| \() -> assert 19 "XIX"
            , test "20" <| \() -> assert 20 "XX"
            , test "28" <| \() -> assert 28 "XXVIII"
            , test "29" <| \() -> assert 29 "XXIX"
            , test "50" <| \() -> assert 50 "L"
            , test "51" <| \() -> assert 51 "LI"
            , test "60" <| \() -> assert 60 "LX"
            , test "79" <| \() -> assert 79 "LXXIX"
            , test "90" <| \() -> assert 90 "XC"
            , test "99" <| \() -> assert 99 "XCIX"
            , test "100" <| \() -> assert 100 "C"
            , test "101" <| \() -> assert 101 "CI"
            , test "102" <| \() -> assert 102 "CII"
            , test "103" <| \() -> assert 103 "CIII"
            , test "104" <| \() -> assert 104 "CIV"
            , test "105" <| \() -> assert 105 "CV"
            , test "106" <| \() -> assert 106 "CVI"
            , test "109" <| \() -> assert 109 "CIX"
            , test "110" <| \() -> assert 110 "CX"
            , test "111" <| \() -> assert 111 "CXI"
            , test "345" <| \() -> assert 345 "CCCXLV"
            , test "456" <| \() -> assert 456 "CDLVI"
            , test "999" <| \() -> assert 999 "CMXCIX"
            , test "1000" <| \() -> assert 1000 "M"
            , test "1001" <| \() -> assert 1001 "MI"
            , test "1002" <| \() -> assert 1002 "MII"
            , test "1009" <| \() -> assert 1009 "MIX"
            , test "1010" <| \() -> assert 1010 "MX"
            , test "1100" <| \() -> assert 1100 "MC"
            , test "1110" <| \() -> assert 1110 "MCX"
            , test "1111" <| \() -> assert 1111 "MCXI"
            , test "1234" <| \() -> assert 1234 "MCCXXXIV"
            , test "1900" <| \() -> assert 1900 "MCM"
            , test "1999" <| \() -> assert 1999 "MCMXCIX"
            , test "2000" <| \() -> assert 2000 "MM"
            , test "3000" <| \() -> assert 3000 "MMM"
            , test "3999" <| \() -> assert 3999 "MMMCMXCIX"
            ]
        , describe "test invalid numbers"
            [ test "0" <|
                \() ->
                    Expect.equal Nothing <| RomanNumeral.arabicToRoman 0
            , test "0" <|
                \() ->
                    Expect.equal Nothing <| RomanNumeral.arabicToRoman 4000
            ]
        , describe "fuzz tests"
            [ fuzz (Fuzz.intRange 1 3999) "never Nothing for valid numbers" <|
                \a ->
                    RomanNumeral.arabicToRoman a |> Expect.notEqual Nothing
            ]
        ]


assert : Int -> String -> Expect.Expectation
assert arabic roman =
    Expect.equal (Just roman) <| RomanNumeral.arabicToRoman arabic
