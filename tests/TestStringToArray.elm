module TestStringToArray exposing (..)

import Expect
import StringToArray exposing (..)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Test StringToArray"
        [ test_dropVerticalBar "|text|" "text"
        , test_dropVerticalBar "|text" "text"
        , test_dropVerticalBar "text|" "text"
        , test_dropVerticalBar "te|xt" "te|xt"
        , test_dropVerticalBar "text" "text"
        ]



-- Test dropVerticalBar


test_dropVerticalBar : String -> String -> Test
test_dropVerticalBar input output =
    describe ("Test dropVerticalBar for " ++ input)
        [ test (input ++ " should be equal to be " ++ output) <|
            \_ ->
                dropVerticalBar input
                    |> Expect.equal output
        ]
