module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Colors as C
import Element exposing (Element, alignRight, centerX, centerY, column, el, fill, height, image, maximum, minimum, none, padding, px, rgb255, row, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Round



---- MODEL ----


type alias CalcResult =
    { change : Change
    , outsideAbsoluteHumidity : Maybe Float
    , insideAbsoluteHumidity : Maybe Float
    }


type Change
    = NotCalculated
    | Unchanged
    | Wetter
    | Drier


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
            ( Just "Bitte nur Zahlen mit einem . als Dezimaltrenner eingeben", Nothing )

        Just num ->
            ( Nothing, Just num )


calculateResultInner : Float -> Float -> Float -> Float -> CalcResult
calculateResultInner outsideTemperature outsideHumidity insideTemperature insideHumidity =
    let
        calculatePower temp =
            (17.67 * temp) / (temp + 243.5)

        calculateAbsoluteHumidity power humidity temperature =
            (6.112 * (e ^ power) * humidity * 2.1674) / (273.15 + temperature)

        outsideAbsoluteHumidity =
            calculateAbsoluteHumidity (calculatePower outsideTemperature) outsideHumidity outsideTemperature

        insideAbsoluteHumidity =
            calculateAbsoluteHumidity (calculatePower insideTemperature) insideHumidity insideTemperature

        change =
            if outsideAbsoluteHumidity == insideAbsoluteHumidity then
                Unchanged

            else if outsideAbsoluteHumidity < insideAbsoluteHumidity then
                Drier

            else
                Wetter
    in
    { change = change
    , insideAbsoluteHumidity = Just <| insideAbsoluteHumidity
    , outsideAbsoluteHumidity = Just <| outsideAbsoluteHumidity
    }


calculateResult : Model -> CalcResult
calculateResult model =
    let
        notCalculated =
            { change = NotCalculated, insideAbsoluteHumidity = Nothing, outsideAbsoluteHumidity = Nothing }
    in
    Maybe.withDefault notCalculated <|
        Maybe.map4 calculateResultInner
            model.outsideTemperature
            model.outsideHumidity
            model.insideTemperature
            model.insideHumidity


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


scaled : Int -> Float
scaled =
    Element.modular 16 1.25


view : Model -> Html Msg
view model =
    Element.layout
        [ Font.size (round <| scaled 2)
        , Font.family [ Font.sansSerif ]
        , width (fill |> maximum 720 |> minimum 720)
        ]
    <|
        column [ width fill ]
            [ titleBar model
            , wrappedRow [ width fill ]
                [ outsideInputs model
                , insideInputs model
                ]
            , bridge
            , results model.calcResult.change
            ]


titleBar : Model -> Element Msg
titleBar model =
    wrappedRow
        [ width fill
        , centerY
        , spacing 30
        , height (scaled 6 |> round |> px)
        , Background.color C.lightblue
        ]
        [ el [ centerX, Font.size (round <| scaled 3), Font.color C.darkblue ] (text "Luftfeuchtigkeit + Fenster öffnen?")
        , el [ alignRight, Font.color C.darkblue ] (text "TODO Impressum")
        ]


bridge : Element Msg
bridge =
    row [ width fill, centerX ]
        [ text "Bild Pfeil nach unten"
        , text "Fenster öffnen"
        ]


results : Change -> Element Msg
results change =
    let
        notCalculated =
            [ text "Not Calculated" ]

        unchanged =
            [ text "Unchanged" ]

        getsWetter =
            [ text "Feuchter Bild"
            , text "Feuchter Text"
            ]

        getsDrier =
            [ text "Trockener Bild"
            , text "Trockener Text"
            ]

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
    row [ width fill, centerX ]
        shownView


absoluteValue : Maybe Float -> Element msg
absoluteValue value =
    el [] (Maybe.withDefault none <| Maybe.map text <| Maybe.map (Round.round 2) value)


scaledToLength : Int -> Element.Length
scaledToLength length =
    scaled length |> round |> px


outsideInputs : Model -> Element Msg
outsideInputs model =
    inputColumn
        { iconUrl = model.icons.outside
        , iconDescription = "Icon showing outside vegetation"
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


insideInputs : Model -> Element Msg
insideInputs model =
    inputColumn
        { iconUrl = model.icons.inside
        , iconDescription = "Icon showing a house"
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


type alias InputConfig =
    { iconUrl : String
    , iconDescription : String
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


inputColumn : InputConfig -> Element Msg
inputColumn c =
    column [ width fill ]
        [ image [ width <| scaledToLength 6, height <| scaledToLength 6, centerX ] { src = c.iconUrl, description = c.iconDescription }
        , numberInput c.onInputTemperature c.currentTemperatureInputValue c.temperaturePlaceholderText c.temperatureLabelText c.temperatureError
        , numberInput c.onInputHumidity c.currentHumidityInputValue c.humidityPlaceholderText c.humidityLabelText c.humidityError
        , absoluteValue c.calculatedAbsoluteHumidity
        ]


numberInput onChange currentValue placeholderText label error =
    let
        errorText =
            el [ Font.color C.red ] <|
                Maybe.withDefault none <|
                    Maybe.map text error
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


main : Program Icons Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
