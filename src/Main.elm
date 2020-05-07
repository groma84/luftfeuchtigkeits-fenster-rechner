module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Element exposing (Element, alignRight, centerX, centerY, column, el, fill, image, none, padding, rgb255, row, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)



---- MODEL ----


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
    , change : Change
    }


init : ( Model, Cmd Msg )
init =
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
      , change = NotCalculated
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


calculateResultInner : Float -> Float -> Float -> Float -> Change
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
    in
    if outsideAbsoluteHumidity == insideAbsoluteHumidity then
        Unchanged

    else if outsideAbsoluteHumidity < insideAbsoluteHumidity then
        Drier

    else
        Wetter


calculateResult : Model -> Change
calculateResult model =
    Maybe.withDefault NotCalculated <|
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
            ( { newModel | change = calculatedResult }, Cmd.none )

        OutsideHumidityInput string ->
            let
                ( error, parsed ) =
                    validateAndParseFloat string

                newModel =
                    { model | outsideHumidityInput = string, outsideHumidityError = error, outsideHumidity = parsed }

                calculatedResult =
                    calculateResult newModel
            in
            ( { newModel | change = calculatedResult }, Cmd.none )

        InsideTemperatureInput string ->
            let
                ( error, parsed ) =
                    validateAndParseFloat string

                newModel =
                    { model | insideTemperatureInput = string, insideTemperatureError = error, insideTemperature = parsed }

                calculatedResult =
                    calculateResult newModel
            in
            ( { newModel | change = calculatedResult }, Cmd.none )

        InsideHumidityInput string ->
            let
                ( error, parsed ) =
                    validateAndParseFloat string

                newModel =
                    { model | insideHumidityInput = string, insideHumidityError = error, insideHumidity = parsed }

                calculatedResult =
                    calculateResult newModel
            in
            ( { newModel | change = calculatedResult }, Cmd.none )



---- VIEW ----


red =
    rgb255 255 0 0


view : Model -> Html Msg
view model =
    Element.layout [] <|
        column []
            [ titleBar model
            , outsideInputs model
            , insideInputs model
            , bridge
            , results model.change
            ]


titleBar : Model -> Element Msg
titleBar model =
    wrappedRow [ width fill, centerY, spacing 30 ]
        [ el [ centerX ] (text "Luftfeuchtigkeit + Fenster öffnen?")
        , el [ alignRight ] (text "Impressum")
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


outsideInputs model =
    column []
        [ text "TODO: image [] { src = \"imageUrl\", description = \"imageDescription\" }"
        , numberInput OutsideTemperatureInput model.outsideTemperatureInput "Temperatur" "°C" model.outsideTemperatureError
        , numberInput OutsideHumidityInput model.outsideHumidityInput "Rel. Luftfeuchtigkeit" "% RH" model.outsideHumidityError
        ]


insideInputs model =
    column []
        [ text "TODO: image [] { src = \"imageUrl\", description = \"imageDescription\" }"
        , numberInput InsideTemperatureInput model.insideTemperatureInput "Temperatur" "°C" model.insideTemperatureError
        , numberInput InsideHumidityInput model.insideHumidityInput "Rel. Luftfeuchtigkeit" "% RH" model.insideHumidityError
        ]


numberInput onChange currentValue placeholderText label error =
    let
        errorText =
            el [ Font.color red ] <|
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


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
