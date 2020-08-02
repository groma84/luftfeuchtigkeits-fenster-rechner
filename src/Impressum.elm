module Impressum exposing (view)

import Html exposing (Html, br, div, h1, h2, p, text)
import Translation exposing (Language(..))


germanView : Html msg
germanView =
    let
        adresse =
            p []
                ([ "Martin Grotz", "Erlanger Straße 60a", "91096 Möhrendorf" ]
                    |> List.map text
                    |> List.intersperse (br [] [])
                )
    in
    div []
        [ h1 [] [ text "Impressum" ]
        , h2 [] [ text "Angaben gemäß § 5 TMG:" ]
        , adresse
        , h2 [] [ text "Kontakt:" ]
        , p [] [ text "E-Mail: martin.grotz@gmx.de" ]
        , h2 [] [ text "Verantwortlich für den Inhalt nach § 55 Abs. 2 RStV:" ]
        , adresse
        , h1 [] [ text "Datenschutz" ]
        , h2 [] [ text "Cookies" ]
        , p [] [ text "Diese Seite verwendet keine Cookies." ]
        , h2 [] [ text "Log-Dateien" ]
        , p [] [ text "Bei jedem Aufruf wird eine anonymisierte IP-Adresse gespeichert - der letzte Block fehlt jeweils, so dass ein Rückschluss auf einen Computer bzw. ein Gerät nicht möglich ist. Außerdem wird gespeichert, mit welchem Browser welche Adresse aufgerufen wurde. Alle Daten werden 14 Tage gespeichert und dann automatisch gelöscht. Eine Auswertung der Daten findet nicht statt." ]
        ]


englishView : Html msg
englishView =
    let
        adresse =
            p []
                ([ "Martin Grotz", "Erlanger Straße 60a", "91096 Möhrendorf" ]
                    |> List.map text
                    |> List.intersperse (br [] [])
                )
    in
    div []
        [ h1 [] [ text "Imprint" ]
        , h2 [] [ text "Data according to § 5 TMG:" ]
        , adresse
        , h2 [] [ text "Contact:" ]
        , p [] [ text "email: martin.grotz@gmx.de" ]
        , h2 [] [ text "Responsible for content according to § 55 Abs. 2 RStV:" ]
        , adresse
        , h1 [] [ text "Privacy" ]
        , h2 [] [ text "Cookies" ]
        , p [] [ text "This page does not use cookies." ]
        , h2 [] [ text "Logging" ]
        , p [] [ text "On every site visit an anonymized IP address is saved - the last block is removed, so you can't pinpoint it to a device. Additionally the used browser is stored. All data is deleted automatically after 14 days. The data will not be analyzed for statistical purposes." ]
        ]


view : Language -> Html msg
view currentLanguage =
    case currentLanguage of
        German ->
            germanView

        English ->
            englishView
