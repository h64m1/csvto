module TestStringToArray exposing (..)

import Expect
import StringToArray exposing (..)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Test StringToArray"
        [ describe "Test dropVerticalBar"
            [ test_dropVerticalBar "|text|" "text"
            , test_dropVerticalBar "|text" "text"
            , test_dropVerticalBar "text|" "text"
            , test_dropVerticalBar "te|xt" "te|xt"
            , test_dropVerticalBar "text" "text"
            ]
        , describe "Test splitStringByVerticalBar"
            [ test_splitStringByVerticalBar "|a|b|c|" [ "a", "b", "c" ]
            , test_splitStringByVerticalBar "|a|b|c" [ "a", "b", "c" ]
            , test_splitStringByVerticalBar "a|b|c|" [ "a", "b", "c" ]
            , test_splitStringByVerticalBar "a|b|c" [ "a", "b", "c" ]
            , test_splitStringByVerticalBar "abc" [ "abc" ]
            ]
        , describe "Test appendTextAlignFormatter"
            [ test_appendTextAlignFormatter [ "a|b|c", "d|e|f", "g|h|i" ] 3 [ "a|b|c", ":--|:--|:--", "d|e|f", "g|h|i" ]
            , test_appendTextAlignFormatter [ "a|b|c" ] 3 [ "a|b|c", ":--|:--|:--" ]
            , test_appendTextAlignFormatter [ "a" ] 1 [ "a", ":--" ]
            , test_appendTextAlignFormatter [ "a|b" ] 2 [ "a|b", ":--|:--" ]
            , test_appendTextAlignFormatter [ "" ] 0 [ "" ]
            ]
        , describe "Test addVerticalBar"
            [ test_addVerticalBar "a|b|c" "|a|b|c|"
            , test_addVerticalBar "a" "|a|"
            , test_addVerticalBar "|a|" "|a|"
            , test_addVerticalBar "|a" "|a|"
            , test_addVerticalBar "a|" "|a|"
            , test_addVerticalBar "" ""
            ]
        ]



-- Test dropVerticalBar : MarkdownText -> String


test_dropVerticalBar : MarkdownText -> String -> Test
test_dropVerticalBar input output =
    describe ("Test dropVerticalBar for " ++ input)
        [ test (input ++ " should be equal to be " ++ output) <|
            \_ ->
                dropVerticalBar input
                    |> Expect.equal output
        ]



-- Test splitStringByVerticalBar : String -> List String


test_splitStringByVerticalBar : String -> List String -> Test
test_splitStringByVerticalBar input output =
    describe ("Test splitStringByVerticalBar for " ++ input)
        [ test (input ++ " should be equal to be " ++ "[" ++ String.join "," output ++ "]") <|
            \_ ->
                splitStringByVerticalBar input
                    |> Expect.equal output
        ]



-- Test appendTextAlignFormatter : List String -> Int -> List String


test_appendTextAlignFormatter : List String -> Int -> List String -> Test
test_appendTextAlignFormatter input n output =
    describe ("Test appendTextAlignFormatter for " ++ "[" ++ String.join "," input ++ "]")
        [ test ("[" ++ String.join "," input ++ "]" ++ " should be equal to be " ++ "[" ++ String.join "," output ++ "]") <|
            \_ ->
                appendTextAlignFormatter input n
                    |> Expect.equal output
        ]



-- addVerticalBar : String -> String


test_addVerticalBar : String -> String -> Test
test_addVerticalBar input output =
    describe ("Test addVerticalBar for " ++ input)
        [ test (input ++ " should be equal to be " ++ output) <|
            \_ ->
                addVerticalBar input
                    |> Expect.equal output
        ]
