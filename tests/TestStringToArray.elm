module TestStringToArray exposing (suite)

import Array exposing (Array)
import Expect
import StringToArray
    exposing
        ( CsvText
        , MarkdownText
        , addVerticalBar
        , appendLineBreak
        , appendTextAlignFormatter
        , convertList2DToCsv
        , convertList2DToMarkdown
        , csvToArrayList
        , dropVerticalBar
        , markdownToArrayList
        , notMarkdownFormat
        , splitStringByVerticalBar
        )
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
        , describe "Test appendLineBreak"
            [ test_appendLineBreak "text" "text\n"
            , test_appendLineBreak "" ""
            ]
        , describe "Test convertList2DToMarkdown"
            [ test_convertList2DToMarkdown [ [ "a", "b", "c" ], [ "d", "e", "f" ] ] "|a|b|c|\n|:--|:--|:--|\n|d|e|f|\n"
            , test_convertList2DToMarkdown [ [ "a", "b", "c" ] ] "|a|b|c|\n|:--|:--|:--|\n"
            , test_convertList2DToMarkdown [] ""
            , test_convertList2DToMarkdown [ [] ] ""
            , test_convertList2DToMarkdown [ [ "a" ] ] "|a|\n|:--|\n"
            ]
        , describe "Test notMarkdowFormat"
            [ test_notMarkdownFormat "a" (not False)
            , test_notMarkdownFormat ":--" (not True)
            , test_notMarkdownFormat "|:--|" (not True)
            , test_notMarkdownFormat ":--|" (not True)
            , test_notMarkdownFormat "|:--" (not True)
            , test_notMarkdownFormat ":-" (not False)
            , test_notMarkdownFormat ":ab" (not False)
            , test_notMarkdownFormat ":a:-" (not False)
            , test_notMarkdownFormat "--:a:" (not False)
            , test_notMarkdownFormat "a:--" (not False)
            , test_notMarkdownFormat "a:--|:--" (not True)
            ]
        , describe "Test markdownToArrayList"
            [ test_markdownToArrayList "|a|b|\n|:--|:--|\n|c|d|" (Array.fromList [ [ "a", "b" ], [ "c", "d" ] ])
            , test_markdownToArrayList "|a|b|" (Array.fromList [ [ "a", "b" ] ])
            , test_markdownToArrayList "|a|\n|:--|" (Array.fromList [ [ "a" ] ])
            , test_markdownToArrayList "" (Array.fromList [ [ "" ] ])
            ]
        , describe "Test convertList2DToCsv"
            [ test_convertList2DToCsv [ [ "a", "b" ] ] "a,b\n"
            , test_convertList2DToCsv [ [ "a", "b" ], [ "c", "d" ] ] "a,b\nc,d\n"
            , test_convertList2DToCsv [ [ "" ] ] ""
            , test_convertList2DToCsv [ [ "a" ] ] "a\n"
            ]
        , describe "Test csvToArrayList"
            [ test_csvToArrayList "a,b" (Array.fromList [ [ "a", "b" ] ])
            , test_csvToArrayList "a,b\nc,d" (Array.fromList [ [ "a", "b" ], [ "c", "d" ] ])
            , test_csvToArrayList "" (Array.fromList [ [ "" ] ])
            , test_csvToArrayList "a\n" (Array.fromList [ [ "a" ], [ "" ] ])
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



-- appendLineBreak : String -> String


test_appendLineBreak : String -> String -> Test
test_appendLineBreak input output =
    describe ("Test appendLineBreak for " ++ input)
        [ test (input ++ " should be equal to be " ++ output) <|
            \_ ->
                appendLineBreak input
                    |> Expect.equal output
        ]



-- convertList2DToMarkdown : List (List String) -> MarkdownText


test_convertList2DToMarkdown : List (List String) -> MarkdownText -> Test
test_convertList2DToMarkdown input output =
    describe ("Test convertList2DToMarkdown for " ++ list2dToString input)
        [ test (list2dToString input ++ " should be equal to be " ++ output) <|
            \_ ->
                convertList2DToMarkdown input
                    |> Expect.equal output
        ]



-- notMarkdownFormat : String -> Bool


test_notMarkdownFormat : String -> Bool -> Test
test_notMarkdownFormat input output =
    describe ("Test notMarkdownFormat for " ++ input)
        [ test (input ++ " should be equal to be " ++ boolToString output) <|
            \_ ->
                notMarkdownFormat input
                    |> Expect.equal output
        ]



-- markdownToArrayList : MarkdownText -> Array2D


test_markdownToArrayList : MarkdownText -> Array (List String) -> Test
test_markdownToArrayList input output =
    describe ("Test markdownToArrayList for " ++ input)
        [ test (input ++ " should be equal to be " ++ array2dToString output) <|
            \_ ->
                markdownToArrayList input
                    |> Expect.equal output
        ]



-- convertList2DToCsv : List2D -> String


test_convertList2DToCsv : List (List String) -> String -> Test
test_convertList2DToCsv input output =
    describe ("Test convertList2DToCsv for " ++ list2dToString input)
        [ test (list2dToString input ++ " should be equal to be " ++ output) <|
            \_ ->
                convertList2DToCsv input
                    |> Expect.equal output
        ]



-- csvToArrayList : CsvText -> Array2D


test_csvToArrayList : CsvText -> Array (List String) -> Test
test_csvToArrayList input output =
    describe ("Test csvToArrayList for " ++ input)
        [ test (input ++ " should be equal to be " ++ array2dToString output) <|
            \_ ->
                csvToArrayList input
                    |> Expect.equal output
        ]



-- string conversion
{-
   2d list -> String
-}


list2dToString : List (List String) -> String
list2dToString list2d =
    let
        list1 =
            list2d |> List.map (String.join ",")

        list2 =
            List.map (\e -> "[" ++ e ++ "]") list1

        text1 =
            String.join "," list2

        text2 =
            "[" ++ text1 ++ "]"
    in
    text2



{-
   array list -> String
-}


array2dToString : Array (List String) -> String
array2dToString array2d =
    let
        list2d =
            array2d
                |> Array.toList

        text =
            list2dToString list2d
    in
    text



{-
   convert Bool to String
   - True -> "True"
   - False -> "False"
-}


boolToString : Bool -> String
boolToString input =
    if input == True then
        "True"

    else
        "False"
