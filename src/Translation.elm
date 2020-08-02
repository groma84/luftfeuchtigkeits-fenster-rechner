module Translation exposing (Language(..), Texts, english, german)


type Language
    = German
    | English


type alias Texts =
    { errorText : String
    , rechner : String
    , impressum : String
    , title : String
    , fensterOeffnen : String
    , berechnungNichtMoeglich : String
    , keineVeraenderung : String
    , feuchter : String
    , trockener : String
    , absoluteFeuchtigkeit : String
    , aussen : String
    , innen : String
    , temperatur : String
    , relativeLuftfeuchtigkeit : String
    }


german : Texts
german =
    { errorText = "Bitte Zahlen mit . als Dezimaltrenner eingeben"
    , rechner = "Rechner"
    , impressum = "Impressum"
    , title = "Luftfeuchtigkeit + Fenster öffnen?"
    , fensterOeffnen = "Fenster öffnen"
    , berechnungNichtMoeglich = "Berechnung noch nicht möglich"
    , keineVeraenderung = "Keine Veränderung"
    , feuchter = "Innen wird es feuchter"
    , trockener = "Innen wird es trockener"
    , absoluteFeuchtigkeit = "Absolute Luftfeuchtigkeit"
    , aussen = "außen"
    , innen = "innen"
    , temperatur = "Temperatur"
    , relativeLuftfeuchtigkeit = "Rel. Luftfeuchtigkeit"
    }


english : Texts
english =
    { errorText = "Please enter only numbers"
    , rechner = "Calculator"
    , impressum = "Imprint"
    , title = "Humidity + an open window?"
    , fensterOeffnen = "Opening the window"
    , berechnungNichtMoeglich = "Calculation not yet possible"
    , keineVeraenderung = "No change"
    , feuchter = "It gets wetter inside"
    , trockener = "It gets drier inside"
    , absoluteFeuchtigkeit = "Absolute humidity"
    , aussen = "outside"
    , innen = "inside"
    , temperatur = "temperature"
    , relativeLuftfeuchtigkeit = "rel. humidity"
    }
