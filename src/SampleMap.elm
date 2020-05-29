module SampleMap exposing (viewSamplesInMap)

import Html exposing (Html)

import Svg exposing (..)
import Svg.Events
import Svg.Attributes exposing (..)

import Habitat2Color exposing (habitat2color)
import Sample exposing (Sample)

mapWidth = 520
mapHeight = 330

gps2coords : Float -> Float -> (Float, Float)
gps2coords lat lon = (mapWidth / 2 + lon / 180.0 * mapWidth / 2, mapHeight / 2 - lat / 90.0 * mapHeight / 2)


viewSamplesInMap : List Sample -> Html a
viewSamplesInMap samples =
    svg
        [ width (String.fromInt mapWidth)
        , height (String.fromInt mapHeight)
        ]
        ([image
            [ width (String.fromInt mapWidth)
            , height (String.fromInt mapHeight)
            , xlinkHref "/assets/World_map_clip_art.svg"
            ]
            []
        ] ++ List.map viewSample samples)

viewSample : Sample -> Svg a
viewSample s =
        let
            (x,y) = gps2coords s.latitude s.longitude
        in circle [ cx (String.fromFloat x)
                  , cy (String.fromFloat y)
                  , r "6"
                  , fill (habitat2color s.habitat)
                  -- , Svg.Events.onMouseOver (ActivateSample (Just s))
                  -- , Svg.Events.onMouseOut (ActivateSample Nothing)
                  ] []

