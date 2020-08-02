module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Calc exposing (CalcResult, Change(..), calculateResult)
import Html exposing (..)
import Html.Attributes exposing (alt, maxlength, placeholder, required, src, step, type_, value)
import Html.Events exposing (onInput)
import MyTachyons exposing (..)
import Round
import Tachyons exposing (classes)
import Tachyons.Classes exposing (..)



---- MODEL ----


type alias Model =
    { outsideTemperatureInput : String
    , outsideTemperatureError : Maybe String
    , outsideTemperature : Maybe Float
    , outsideHumidityInput : String
    , outsideHumidityError : Maybe String
    , outsideHumidity : Maybe Float
    , insideTemperatureInput : String
    , insideTemperatureError : Maybe String
    , insideTemperature : Maybe Float
    , insideHumidityInput : String
    , insideHumidityError : Maybe String
    , insideHumidity : Maybe Float
    , calcResult : CalcResult
    , icons : Icons
    }


type alias Icons =
    { down_arrow : String
    , dry : String
    , inside : String
    , opening_window : String
    , outside : String
    , wet : String
    , left_right_arrow : String
    , question_mark : String
    }


type alias InputConfig =
    { iconUrl : String
    , iconDescription : String
    , rowLabel : String
    , onInputTemperature : String -> Msg
    , onInputHumidity : String -> Msg
    , currentTemperatureInputValue : String
    , currentHumidityInputValue : String
    , temperaturePlaceholderText : String
    , humidityPlaceholderText : String
    , temperatureLabelText : String
    , humidityLabelText : String
    , temperatureError : Maybe String
    , humidityError : Maybe String
    , calculatedAbsoluteHumidity : Maybe Float
    }


init : Icons -> ( Model, Cmd Msg )
init icons =
    ( { outsideTemperatureInput = ""
      , outsideTemperatureError = Nothing
      , outsideTemperature = Nothing
      , outsideHumidityInput = ""
      , outsideHumidityError = Nothing
      , outsideHumidity = Nothing
      , insideTemperatureInput = ""
      , insideTemperatureError = Nothing
      , insideTemperature = Nothing
      , insideHumidityInput = ""
      , insideHumidityError = Nothing
      , insideHumidity = Nothing
      , calcResult = { change = NotCalculated, insideAbsoluteHumidity = Nothing, outsideAbsoluteHumidity = Nothing }
      , icons = icons
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = OutsideTemperatureInput String
    | OutsideHumidityInput String
    | InsideTemperatureInput String
    | InsideHumidityInput String


validateAndParseFloat : String -> ( Maybe String, Maybe Float )
validateAndParseFloat input =
    let
        parsedInput =
            String.toFloat input
    in
    case parsedInput of
        Nothing ->
            ( Just "Bitte Zahlen mit . als Dezimaltrenner eingeben", Nothing )

        Just num ->
            ( Nothing, Just num )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OutsideTemperatureInput string ->
            let
                ( error, parsed ) =
                    validateAndParseFloat string

                newModel =
                    { model | outsideTemperatureInput = string, outsideTemperatureError = error, outsideTemperature = parsed }

                calculatedResult =
                    calculateResult newModel
            in
            ( { newModel | calcResult = calculatedResult }, Cmd.none )

        OutsideHumidityInput string ->
            let
                ( error, parsed ) =
                    validateAndParseFloat string

                newModel =
                    { model | outsideHumidityInput = string, outsideHumidityError = error, outsideHumidity = parsed }

                calculatedResult =
                    calculateResult newModel
            in
            ( { newModel | calcResult = calculatedResult }, Cmd.none )

        InsideTemperatureInput string ->
            let
                ( error, parsed ) =
                    validateAndParseFloat string

                newModel =
                    { model | insideTemperatureInput = string, insideTemperatureError = error, insideTemperature = parsed }

                calculatedResult =
                    calculateResult newModel
            in
            ( { newModel | calcResult = calculatedResult }, Cmd.none )

        InsideHumidityInput string ->
            let
                ( error, parsed ) =
                    validateAndParseFloat string

                newModel =
                    { model | insideHumidityInput = string, insideHumidityError = error, insideHumidity = parsed }

                calculatedResult =
                    calculateResult newModel
            in
            ( { newModel | calcResult = calculatedResult }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ classes [ sans_serif, mw6 ] ]
        [ titleBar
        , div []
            [ outsideInputs model
            , insideInputs model
            ]
        , bridge model.icons
        , results model.icons model.calcResult.change
        ]


titleBar : Html Msg
titleBar =
    header
        [ classes [ flex, flex_auto, flex_row, justify_between, items_center, Tachyons.Classes.h2, white, bg_azure, pa2 ] ]
        [ div [ classes [ di ] ] [ text "Luftfeuchtigkeit + Fenster öffnen?" ]
        , div [ classes [ di ] ] [ text "TODO Impressum" ]
        ]


bridge : Icons -> Html Msg
bridge icons =
    div [ classes [ flex, flex_row, justify_center ] ]
        [ img
            [ src icons.down_arrow
            , alt "Pfeil nach unten"
            , classes [ mw2 ]
            ]
            []
        , img
            [ src icons.opening_window
            , alt "Offenes Fenster"
            , classes [ mw2 ]
            ]
            []
        ]


results : Icons -> Change -> Html Msg
results icons change =
    let
        iconAndText iconSrc iconAlt txt =
            [ img [ src iconSrc, alt iconAlt, classes [ mw2 ] ] []
            , span [ classes [ f4 ] ] [ text txt ]
            ]

        notCalculated =
            iconAndText icons.question_mark "Fragezeichen" "Berechnung noch nicht möglich"

        unchanged =
            iconAndText icons.left_right_arrow "Pfeil nach links und rechts" "Keine Veränderung"

        getsWetter =
            iconAndText icons.wet "Regenwolke" "Innen wird es feuchter"

        getsDrier =
            iconAndText icons.dry "Kaktus" "Innen wird es trockener"

        shownView =
            case change of
                NotCalculated ->
                    notCalculated

                Unchanged ->
                    unchanged

                Wetter ->
                    getsWetter

                Drier ->
                    getsDrier
    in
    div [ classes [ flex, flex_row, justify_center, items_center, mv3 ] ]
        shownView


absoluteValue : Maybe Float -> Html msg
absoluteValue value =
    let
        el txt =
            span [] [ text ("Absolute Feuchtigkeit: " ++ txt ++ " g/m³") ]
    in
    Maybe.withDefault (text "") <|
        Maybe.map (\v -> el <| Round.round 2 v) value


outsideInputs : Model -> Html Msg
outsideInputs model =
    inputRow
        { iconUrl = model.icons.outside
        , iconDescription = "Icon showing outside vegetation"
        , rowLabel = "außen"
        , onInputTemperature = OutsideTemperatureInput
        , onInputHumidity = OutsideHumidityInput
        , currentTemperatureInputValue = model.outsideTemperatureInput
        , currentHumidityInputValue = model.outsideHumidityInput
        , temperaturePlaceholderText = "Temperatur"
        , humidityPlaceholderText = "Rel. Luftfeuchtigkeit"
        , temperatureLabelText = "°C"
        , humidityLabelText = "% RH"
        , temperatureError = model.outsideTemperatureError
        , humidityError = model.outsideHumidityError
        , calculatedAbsoluteHumidity = model.calcResult.outsideAbsoluteHumidity
        }


insideInputs : Model -> Html Msg
insideInputs model =
    inputRow
        { iconUrl = model.icons.inside
        , iconDescription = "Icon showing a house"
        , rowLabel = "innen"
        , onInputTemperature = InsideTemperatureInput
        , onInputHumidity = InsideHumidityInput
        , currentTemperatureInputValue = model.insideTemperatureInput
        , currentHumidityInputValue = model.insideHumidityInput
        , temperaturePlaceholderText = "Temperatur"
        , humidityPlaceholderText = "Rel. Luftfeuchtigkeit"
        , temperatureLabelText = "°C"
        , humidityLabelText = "% RH"
        , temperatureError = model.insideTemperatureError
        , humidityError = model.insideHumidityError
        , calculatedAbsoluteHumidity = model.calcResult.insideAbsoluteHumidity
        }


inputRow : InputConfig -> Html Msg
inputRow c =
    section [ classes [ flex, flex_row, flex_auto, ml1, mt1, mb3 ] ]
        [ div [ classes [ w_10, flex, flex_column, items_center ] ]
            [ img [ src c.iconUrl, alt c.iconDescription ] []
            , span [] [ text c.rowLabel ]
            ]
        , div [ classes [ mh2 ] ]
            [ numberInput c.onInputTemperature c.currentTemperatureInputValue c.temperaturePlaceholderText c.temperatureLabelText "-30" "0.1" c.temperatureError
            , numberInput c.onInputHumidity c.currentHumidityInputValue c.humidityPlaceholderText c.humidityLabelText "0" "1" c.humidityError
            , absoluteValue c.calculatedAbsoluteHumidity
            ]
        ]


numberInput : (String -> Msg) -> String -> String -> String -> String -> String -> Maybe String -> Html Msg
numberInput onChange currentValue placeholderText unitSymbol minValue stepValue error =
    let
        errorText =
            let
                el txt =
                    div [ classes [ mt1, red, Tachyons.Classes.i, f7 ] ] [ text txt ]
            in
            Maybe.withDefault (text "") <|
                Maybe.map el error
    in
    div [ classes [ pv1 ] ]
        [ input
            [ classes [ w4 ]
            , value currentValue
            , type_ "number"
            , required True
            , Html.Attributes.min minValue
            , Html.Attributes.max "100"
            , step stepValue
            , placeholder placeholderText
            , maxlength 10
            , onInput onChange
            ]
            []
        , span [] [ text unitSymbol ]
        , errorText
        ]



---- PROGRAM ----


main : Program Icons Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
