module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Element exposing (Element, alignRight, centerX, centerY, column, el, fill, image, none, padding, rgb255, row, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)



---- MODEL ----


type alias Model =
    { temperatureInput : String
    , humidityInput : String
    , temperatureError : Maybe String
    , temperature : Maybe Int
    }


init : ( Model, Cmd Msg )
init =
    ( { temperatureInput = ""
      , humidityInput = ""
      , temperatureError = Nothing
      , temperature = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = TemperatureInput String
    | HumidityInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TemperatureInput string ->
            let
                parsedInput =
                    String.toInt string

                newModel =
                    case parsedInput of
                        Nothing ->
                            { model | temperatureError = Just "Bitte nur ganze Zahlen eingeben", temperatureInput = string }

                        Just num ->
                            { model | temperatureError = Nothing, temperatureInput = string, temperature = Just num }
            in
            ( newModel, Cmd.none )

        HumidityInput string ->
            ( model, Cmd.none )



---- VIEW ----


red =
    rgb255 255 0 0


view : Model -> Html Msg
view model =
    Element.layout [] <|
        column []
            [ titleBar model
            , inputs model
            ]


titleBar : Model -> Element Msg
titleBar model =
    wrappedRow [ width fill, centerY, spacing 30 ]
        [ el [ centerX ] (text "Luftfeuchtigkeit + Fenster öffnen?")
        , el [ alignRight ] (text "Impressum")
        ]


inputs model =
    column []
        [ text "TODO: image [] { src = \"x\", description = \"y\" }"
        , numberInput TemperatureInput model.temperatureInput "Temperatur" "°C" model.temperatureError
        ]


numberInput onChange currentValue placeholderText label error =
    let
        errorText =
            case error of
                Nothing ->
                    none

                Just e ->
                    el [ Font.color red ] (text e)
    in
    column []
        [ errorText
        , Input.text []
            { onChange = onChange
            , text = currentValue
            , placeholder = Just <| Input.placeholder [] (text placeholderText)
            , label = Input.labelRight [] (text label)
            }
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
