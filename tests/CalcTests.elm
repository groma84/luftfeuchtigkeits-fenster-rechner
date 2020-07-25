module CalcTests exposing (calcTests)

import Calc exposing (calculateResult)
import Expect
import Fuzz exposing (..)
import Test exposing (..)


fuzzFourMaybeFloats =
    fuzz2 (Fuzz.tuple ( Fuzz.maybe Fuzz.float, Fuzz.maybe Fuzz.float ))
        (Fuzz.tuple ( Fuzz.maybe Fuzz.float, Fuzz.maybe Fuzz.float ))


isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False


fuzzFourFloats =
    fuzz2 (Fuzz.tuple ( Fuzz.float, Fuzz.float ))
        (Fuzz.tuple ( Fuzz.float, Fuzz.float ))


calcTests : Test
calcTests =
    describe "calculateResult"
        [ describe "example based tests"
            [ test "Calculates correct result for outside wet, inside dry" <|
                \_ ->
                    let
                        actual =
                            calculateResult
                                { outsideTemperature = Just 20
                                , outsideHumidity = Just 50
                                , insideTemperature = Just 20
                                , insideHumidity = Just 20
                                }
                    in
                    Expect.equal
                        { change = Calc.Wetter
                        , outsideAbsoluteHumidity = Just 8.639091242147579
                        , insideAbsoluteHumidity = Just 3.4556364968590314
                        }
                        actual
            , test "Calculates correct result for outside dry, inside wet" <|
                \_ ->
                    let
                        actual =
                            calculateResult
                                { outsideTemperature = Just 20
                                , outsideHumidity = Just 20
                                , insideTemperature = Just 20
                                , insideHumidity = Just 50
                                }
                    in
                    Expect.equal
                        { change = Calc.Drier
                        , outsideAbsoluteHumidity = Just 3.4556364968590314
                        , insideAbsoluteHumidity = Just 8.639091242147579
                        }
                        actual
            , test "Calculates correct result for outside same as inside" <|
                \_ ->
                    let
                        actual =
                            calculateResult
                                { outsideTemperature = Just 20
                                , outsideHumidity = Just 20
                                , insideTemperature = Just 20
                                , insideHumidity = Just 20
                                }
                    in
                    Expect.equal
                        { change = Calc.Unchanged
                        , outsideAbsoluteHumidity = Just 3.4556364968590314
                        , insideAbsoluteHumidity = Just 3.4556364968590314
                        }
                        actual
            ]
        , describe
            "fuzz tests"
            [ fuzzFourMaybeFloats
                "if any input is Nothing no result is calculated"
              <|
                \( a, b ) ( c, d ) ->
                    let
                        actual =
                            calculateResult
                                { outsideTemperature = a
                                , outsideHumidity = b
                                , insideTemperature = c
                                , insideHumidity = d
                                }

                        expectedResult =
                            Maybe.map4 (\_ _ _ _ -> Expect.notEqual Calc.NotCalculated) a b c d
                                |> Maybe.withDefault (Expect.equal Calc.NotCalculated)
                    in
                    expectedResult actual.change
            , fuzzFourFloats
                "if all inputs have values a result is calculated"
              <|
                \( a, b ) ( c, d ) ->
                    let
                        actual =
                            calculateResult
                                { outsideTemperature = Just a
                                , outsideHumidity = Just b
                                , insideTemperature = Just c
                                , insideHumidity = Just d
                                }

                        hasBeenCalculated act =
                            Expect.true "Change must have been calculated" (act.change /= Calc.NotCalculated)

                        outsideAbsoluteHumidityCalculated act =
                            Expect.true "outsideAbsoluteHumidity must have been calculated" <| isJust act.outsideAbsoluteHumidity

                        insideAbsoluteHumidityCalculated act =
                            Expect.true "insideAbsoluteHumidity must have been calculated" <| isJust act.insideAbsoluteHumidity
                    in
                    Expect.all [ hasBeenCalculated, outsideAbsoluteHumidityCalculated, insideAbsoluteHumidityCalculated ] actual
            ]
        ]
