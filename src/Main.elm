module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, a, div, main_, nav, section, span, table, tbody, td, text, textarea, thead, tr)
import Html.Attributes exposing (class, href, id, placeholder, target, value)
import Html.Events exposing (onClick, onInput)
import StringToArray exposing (CsvText, MarkdownText)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { csvInput : CsvText -- csvの入力値
    , markdownInput : MarkdownText -- markdownの入力値
    , arrayInput : Array (List String) -- csv入力を変換した2次元配列
    , showView : ViewDisplayState -- viewの表示状態
    }



{-
   csv, markdown領域の表示管理フラグ
   defaultではcsvを表示
-}


type ViewDisplayState
    = Csv
    | Markdown


init : () -> ( Model, Cmd Msg )
init _ =
    ( { csvInput = ""
      , markdownInput = ""
      , arrayInput = Array.fromList []
      , showView = Csv
      }
    , Cmd.none
    )


type Msg
    = CsvInput String
    | MarkdownInput String
    | ShowCsv
    | ShowMarkdown


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CsvInput input ->
            let
                arrayInput =
                    StringToArray.csvToArrayList input
            in
            ( { model
                | csvInput = input
                , arrayInput = arrayInput
                , markdownInput = StringToArray.arrayListToMarkdown arrayInput
              }
            , Cmd.none
            )

        MarkdownInput input ->
            let
                arrayInput =
                    StringToArray.markdownToArrayList input
            in
            ( { model
                | markdownInput = input
                , arrayInput = arrayInput
                , csvInput = StringToArray.arrayListToCsv arrayInput
              }
            , Cmd.none
            )

        ShowCsv ->
            ( { model | showView = Csv }, Cmd.none )

        ShowMarkdown ->
            ( { model | showView = Markdown }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    main_ [ id "app" ]
        [ nav [ id "left-nav" ] []
        , mainContainer model
        ]



-- MAIN CONTAINER


mainContainer : Model -> Html Msg
mainContainer model =
    section [ class "main-container" ]
        [ div [ class "title" ] []
        , headerView model
        , div [ class "preview-title-container" ]
            [ div [ class "preview-title" ] [ text "preview" ]
            ]
        , inputContainer model
        , previewContainer model
        , footerView
        ]



-- main header


headerView : Model -> Html Msg
headerView model =
    div [ class "tab-container" ]
        [ tabView Csv model ShowCsv "csv"
        , tabView Markdown model ShowMarkdown "markdown"
        ]



-- footer


footerView : Html Msg
footerView =
    div [ class "footer" ]
        [ text "created by "
        , footerLink "https://github.com/h64m1" "h64m1"
        , text " | "
        , footerLink "https://github.com/h64m1/csvto" "repository"
        ]



-- footer link


footerLink : String -> String -> Html msg
footerLink link description =
    a [ href link, target "_blank" ] [ text description ]



-- タブ表示


tabView : ViewDisplayState -> Model -> Msg -> String -> Html Msg
tabView state model msg title =
    a [ class "tab", href "#" ]
        [ span [ class (tabClassName state model), onClick msg ] [ text title ]
        ]



{-
   tabの選択状態によって表示を切り替えるためのクラス指定
   - selected: 選択状態
   - not-selected: 選択されてない状態
-}


tabClassName : ViewDisplayState -> Model -> String
tabClassName state model =
    if state == model.showView then
        "tab-selected"

    else
        "tab-not-selected"



{-
   入力可能な領域を表示するcontainer
   showViewの状態によって表示領域を変える
   csv: csv入力エリアを表示
   markdown: markdown入力エリアを表示
-}


inputContainer : Model -> Html Msg
inputContainer model =
    case model.showView of
        Csv ->
            csvContainer model

        Markdown ->
            markdownContainer model



-- CSV INPUT


csvContainer : Model -> Html Msg
csvContainer model =
    div [ class "csv-container" ]
        [ textarea [ class "input-area", placeholder "csv: a, b, c, ...", value model.csvInput, onInput CsvInput ] [ text "" ]
        ]



-- MARKDOWN INPUT


markdownContainer : Model -> Html Msg
markdownContainer model =
    div [ class "markdown-container" ]
        [ textarea [ class "input-area", placeholder "write markdown table", value model.markdownInput, onInput MarkdownInput ] [ text "" ]
        ]



-- PREVIEW


previewContainer : Model -> Html Msg
previewContainer model =
    div [ class "preview-container" ] [ previewTableView model ]



-- プレビュー用のテーブル


previewTableView : Model -> Html Msg
previewTableView model =
    table [ class "preview-area" ]
        [ tableHeaderView model
        , tableBodyView model
        ]



-- タイトル行


tableHeaderView : Model -> Html Msg
tableHeaderView model =
    thead [ class "preview-header" ] [ rowView model 0 ]



-- テーブル本体


tableBodyView : Model -> Html Msg
tableBodyView model =
    tbody []
        (rowViewList model 1 (Array.length model.arrayInput - 1))



-- テーブルの本体中身
{- 以下のような形
   [ tr [] (trElement model 1)
   , tr [] (trElement model 2)
   , tr [] (trElement model 3)
   ]
-}


rowViewList : Model -> Int -> Int -> List (Html msg)
rowViewList model start end =
    List.range start end
        |> List.map (rowView model)



-- テーブルの行を描画


rowView : Model -> Int -> Html msg
rowView model index =
    tr [] (trElement model index)



-- 2次元配列の特定要素からテーブルのDOMを生成


trElement : Model -> Int -> List (Html msg)
trElement model index =
    Maybe.withDefault []
        (Array.get index
            (model.arrayInput
                |> Array.map (List.map tdElement)
            )
        )



-- TD要素を作成


tdElement : String -> Html msg
tdElement input =
    td [] [ text input ]



-- port saveArray : Array (List String) -> Cmd msg
