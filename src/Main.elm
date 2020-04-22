port module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, main_, nav, section, table, tbody, td, text, textarea, thead, tr)
import Html.Attributes exposing (class, id, placeholder, value)
import Html.Events exposing (onClick, onInput)


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
    { csvInput : String -- csvの入力値
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
      , arrayInput = Array.fromList []
      , showView = Csv
      }
    , Cmd.none
    )


type Msg
    = CsvInput String
    | ShowCsv
    | ShowMarkdown


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CsvInput input ->
            ( { model
                | csvInput = input
                , arrayInput =
                    let
                        -- 1. 改行コードで分割、2. カンマで分割、で行ごとに配列化
                        array =
                            input
                                |> String.lines
                                |> List.map (String.split ",")
                                |> Array.fromList

                        -- _ =
                        --     Debug.log "array" array
                    in
                    array
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
        [ inputContainer model
        , previewContainer model
        ]



{-
   入力可能な領域を表示するcontainer
   showViewの状態によって表示領域を変える
   csv: csv入力エリアを表示
   markdown: markdown入力エリアを表示（TODO: 一旦csvエリアを表示、markdown表示エリアは要実装）
-}


inputContainer : Model -> Html Msg
inputContainer model =
    case model.showView of
        Csv ->
            csvContainer model

        Markdown ->
            csvContainer model



-- CSV INPUT


csvContainer : Model -> Html Msg
csvContainer model =
    div [ class "csv-container" ]
        [ div [ class "title" ] [ text "csv" ] -- button [ onClick Preview ] [ text "preview" ]
        , textarea [ class "csv-area", placeholder "csv: a, b, c, ...", value model.csvInput, onInput CsvInput ] [ text "" ]
        ]



-- PREVIEW


previewContainer : Model -> Html Msg
previewContainer model =
    div [ class "preview-container" ]
        [ div [ class "title" ] [ text "preview" ]
        , previewTableView model
        ]



-- プレビュー用のテーブル


previewTableView : Model -> Html Msg
previewTableView model =
    table [ class "preview-area" ]
        [ headerView model
        , bodyView model
        ]



-- タイトル行


headerView : Model -> Html Msg
headerView model =
    thead [ class "preview-header" ] [ rowView model 0 ]



-- テーブル本体


bodyView : Model -> Html Msg
bodyView model =
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


port saveArray : Array (List String) -> Cmd msg
