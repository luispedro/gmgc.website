module SampleMap exposing (viewSamplesInMap, viewHabitats)

import Dict as Dict
import Html exposing (Html)

import Svg exposing (..)
import Svg.Events
import Svg.Attributes exposing (..)

import Habitat2Color exposing (habitat2color)
import Sample exposing (Sample)

mapWidth = 520
mapHeight = 330

-- BUG: There are some issues here: the dots are not in the right location!
gps2coords : Float -> Float -> (Float, Float)
gps2coords lat lon = (mapWidth / 2 + lon / 180.0 * mapWidth / 2, mapHeight / 2 - lat / 90.0 * mapHeight / 2)


viewSamplesInMap : List Sample -> (Maybe Sample -> a) -> Html a
viewSamplesInMap samples onSampleClick =
    svg
        [ width (String.fromInt mapWidth)
        , height (String.fromInt mapHeight)
        ]
        ([image
            [ width (String.fromInt mapWidth)
            , height (String.fromInt mapHeight)
            , xlinkHref "assets/World_map_clip_art.svg"
            ]
            []
        ] ++ List.map (viewSample onSampleClick) samples )

-- The caller can specify a function to be called when a sample is clicked
viewSample : (Maybe Sample -> a) -> Sample -> Svg a
viewSample onSampleClick s =
        let
            (x,y) = gps2coords s.latitude s.longitude
        in circle [ cx (String.fromFloat x)
                  , cy (String.fromFloat y)
                  , r "6"
                  , fill (habitat2color s.habitat)
                  , Svg.Events.onClick (onSampleClick (Just s))
                  ] []

-- The caller can specify a function to be called when a habitat is clicked
viewHabitats : List String
                    -> Maybe String
                    -> (Maybe String -> a)
                    -> Html a
viewHabitats habitats hfilter clickMessage =
    let
        habitatCounts = countHabitats habitats
        filtered = Dict.toList habitatCounts |>
            List.sortBy (\(h,c) -> 0 - c) |>
            List.filter (\(h,_) -> h /= "-") |> -- take out ProGenomes2
            List.take 6
        topMargin = 60
        barHeight = 20
        betweenBarsSpacing = 30

    in svg
        [ width "500"
        , height (String.fromInt <| topMargin + 30 * (List.length filtered))
        ]
        (filtered |> List.indexedMap (\ix (h,c) ->
            let
                isActive = (hfilter == Just h)
            in
                (Svg.g [ Svg.Events.onClick (clickMessage (if isActive then Nothing else (Just h))) ]
                    {- Draw a horizontal histogram
                    -}
                    [Svg.rect
                        [x "0"
                        , y (String.fromInt <| topMargin + betweenBarsSpacing * ix)
                        , fill (if isActive
                                    then "#cc9933"
                                    else "#cccccc")
                        , fillOpacity "0.1"
                        , rx "4"
                        , stroke "#000000"
                        , width "116"
                        , height "25"
                        ]
                        [ ]
                    ,Svg.text_
                        [x "4"
                        , y (String.fromFloat <| topMargin + betweenBarsSpacing * (toFloat ix + 0.5))
                        , fontSize "12px"]
                        [ Svg.text h ]
                    ,rect
                        [ x "140"
                        , y (String.fromInt (topMargin + betweenBarsSpacing * ix))
                        , width (String.fromInt <| c * 10)
                        , rx "2"
                        , height (String.fromInt barHeight)
                        , fill (habitat2color h)
                        ] []
                    ,Svg.text_
                        [ x "120"
                        , y (String.fromFloat <| topMargin + betweenBarsSpacing * (toFloat ix + 0.5))
                        , color "#ffffff"
                        , fontSize "14px"]
                        [ Svg.text (String.fromInt c) ]
                    ]
                    )))

countHabitats : List String -> Dict.Dict String Int
countHabitats hs =
    let
        add1 a = Just <| 1 + Maybe.withDefault 0 a
    in List.foldl (\c h -> Dict.update c add1 h) Dict.empty hs

