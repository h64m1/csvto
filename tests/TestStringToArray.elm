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
        ]



-- Test dropVerticalBar : MarkdownText -> String


test_dropVerticalBar : String -> String -> Test
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
