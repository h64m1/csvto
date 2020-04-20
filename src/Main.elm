module Main exposing (main)

import Browser
import Html exposing (Html, div, main_, nav, section, text, textarea)
import Html.Attributes exposing (class, id, placeholder)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Model
    = Failure
    | Loading
    | Success


init : () -> ( Model, Cmd Msg )
init _ =
    ( Failure, Cmd.none )


type Msg
    = Msg1
    | Msg2


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg1 ->
            ( model, Cmd.none )

        Msg2 ->
            ( model, Cmd.none )


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


mainContainer : Model -> Html msg
mainContainer model =
    section [ class "main-container" ]
        [ div [ class "title" ] [ text "csv" ]
        , textarea [ class "csv-area", placeholder "csv: a, b, c, ..." ] [ text "" ]
        ]
