module Sample exposing (..)
import Json.Decode as Json

type alias Sample =
    { name : String
    , latitude : Float
    , longitude : Float
    , habitat : String
    }

decode : Json.Decoder Sample
decode = Json.map4
    Sample
        (Json.field "name" Json.string)
        (Json.field "latitude" Json.float)
        (Json.field "longitude" Json.float)
        (Json.field "habitat" Json.string)
