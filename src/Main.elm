port module Main exposing (main)

import Browser
import Debug
import Html exposing (Html, button, div, main_, nav, section, table, tbody, td, text, textarea, tr)
import Html.Attributes exposing (class, id, placeholder, style, value)
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
    { input : String
    , arrayInput : List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { input = "", arrayInput = [] }, Cmd.none )


type Msg
    = Input String
    | Preview


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input input ->
            ( { model
                | input =
                    let
                        _ =
                            Debug.log "input" input
                    in
                    input
                , arrayInput =
                    let
                        array =
                            input |> String.split ","

                        _ =
                            Debug.log "array" array
                    in
                    array
              }
            , Cmd.none
            )

        Preview ->
            ( model, saveArray model.arrayInput )


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
        [ csvContainer model
        , previewContainer model
        ]



-- CSV INPUT


csvContainer : Model -> Html Msg
csvContainer model =
    div [ class "csv-container" ]
        [ div [ class "title" ] [ text "csv", button [ onClick Preview ] [ text "preview" ] ]
        , textarea [ class "csv-area", placeholder "csv: a, b, c, ...", value model.input, onInput Input ] [ text "" ]
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
        [ tbody []
            [ tr []
                (model.arrayInput |> List.map tdElement)
            ]
        ]


tdElement : String -> Html msg
tdElement input =
    td [] [ text input ]


port saveArray : List String -> Cmd msg
