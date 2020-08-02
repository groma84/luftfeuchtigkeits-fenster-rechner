module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Calc exposing (CalcResult, Change(..), calculateResult)
import Html exposing (..)
import Html.Attributes exposing (alt, autofocus, maxlength, name, placeholder, required, src, step, type_, value)
import Html.Events exposing (onClick, onInput)
import Impressum
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
    , showImpressum : Bool
    }


type alias Icons =
    { down_arrow : String
    , dry : String
    , inside : String
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
    , autofocusFirstInput : Bool
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
      , showImpressum = False
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = OutsideTemperatureInput String
    | OutsideHumidityInput String
    | InsideTemperatureInput String
    | InsideHumidityInput String
    | ShowImpressum Bool


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

        ShowImpressum val ->
            ( { model | showImpressum = val }, Cmd.none )



---- VIEW ----


calculatorView : Model -> List (Html Msg)
calculatorView model =
    [ div []
        [ outsideInputs model
        , insideInputs model
        ]
    , bridge model.icons
    , results model.icons model.calcResult.change
    ]


view : Model -> Html Msg
view model =
    let
        shownView =
            if model.showImpressum then
                [ Impressum.view ]

            else
                calculatorView model
    in
    div [ classes [ sans_serif, mw6 ] ]
        (List.concat [ [ titleBar model.showImpressum ], shownView ])


titleBar : Bool -> Html Msg
titleBar impressumShown =
    let
        impressumButtonText =
            if impressumShown then
                "Rechner"

            else
                "Impressum"
    in
    header
        [ classes [ flex, flex_auto, flex_row, justify_between, items_center, Tachyons.Classes.h2, white, bg_azure, pa2 ] ]
        [ div [ classes [ di ] ] [ text "Luftfeuchtigkeit + Fenster öffnen?" ]
        , div [ classes [ di ] ] [ button [ classes [ pointer, w4 ], type_ "button", onClick <| ShowImpressum (not impressumShown), name impressumButtonText ] [ text impressumButtonText ] ]
        ]


bridge : Icons -> Html Msg
bridge icons =
    div [ classes [ flex, flex_row, justify_center, items_center ] ]
        [ img
            [ src icons.down_arrow
            , alt "Pfeil nach unten"
            , classes [ mw2 ]
            ]
            []
        , span [ classes [ f6, ph2 ] ] [ text "Fenster öffnen" ]
        , img
            [ src icons.down_arrow
            , alt "Pfeil nach unten"
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
            , img [ src iconSrc, alt iconAlt, classes [ mw2 ] ] []
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
        , autofocusFirstInput = True
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
        , autofocusFirstInput = False
        }


inputRow : InputConfig -> Html Msg
inputRow c =
    section [ classes [ flex, flex_row, flex_auto, ml1, mt1, mb3 ] ]
        [ div [ classes [ w_10, flex, flex_column, items_center ] ]
            [ img [ src c.iconUrl, alt c.iconDescription ] []
            , span [] [ text c.rowLabel ]
            ]
        , div [ classes [ mh2 ] ]
            [ numberInput c.onInputTemperature c.currentTemperatureInputValue c.temperaturePlaceholderText c.temperatureLabelText "-30" "0.1" c.autofocusFirstInput c.temperatureError
            , numberInput c.onInputHumidity c.currentHumidityInputValue c.humidityPlaceholderText c.humidityLabelText "0" "1" c.autofocusFirstInput c.humidityError
            , absoluteValue c.calculatedAbsoluteHumidity
            ]
        ]


numberInput : (String -> Msg) -> String -> String -> String -> String -> String -> Bool -> Maybe String -> Html Msg
numberInput onChange currentValue placeholderText unitSymbol minValue stepValue autofocused error =
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
            , autofocus autofocused
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
