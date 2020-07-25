module Calc exposing (CalcResult, Change(..), calculateResult)


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


type Change
    = NotCalculated
    | Unchanged
    | Wetter
    | Drier


type alias CalcResult =
    { change : Change
    , outsideAbsoluteHumidity : Maybe Float
    , insideAbsoluteHumidity : Maybe Float
    }


calculateResult :
    { a
        | outsideTemperature : Maybe Float
        , outsideHumidity : Maybe Float
        , insideTemperature : Maybe Float
        , insideHumidity : Maybe Float
    }
    -> CalcResult
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
