module SearchResults exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (href)

import Svg exposing (..)
import Svg.Events
import Svg.Attributes exposing (..)

import Element exposing (Element)
import Element as E
import Element.Input as EI
import Element.Font as Font

import Bootstrap.CDN as CDN
import Bootstrap.Table as Table

import NiceRound exposing (niceRound)
import Sample exposing (Sample)
import GMGCv1samples exposing (gmgcV1samples)
import TestHits exposing (testHits)


main : Program () Model Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }


type alias Hit =
    { evalue : Float
    , bitscore : Float
    , geneID : String
    , taxon : String
    , habitat : String
    , origin : Maybe Sample
    }

type alias Model =
    { hits : List Hit
    , cutoffIx : Int
    , activeSample : Maybe Sample
    }

activeHits : Model -> List Hit
activeHits model = List.take model.cutoffIx model.hits

init : () -> ( Model, Cmd Msg )
init () =
  ( { hits = buildTestHits
    , cutoffIx = 50
    , activeSample = Nothing
    }
  , Cmd.none
  )


type Msg
    = NoOp
    | UpdateHitFilter Float
    | ActivateSample (Maybe Sample)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
    (UpdateHitFilter nv ) ->
          ( { model | cutoffIx = round nv }
          , Cmd.none )
    ActivateSample s ->
          ( { model | activeSample = s }
          , Cmd.none )
    _ -> ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Results for your query"
    , body = [ CDN.stylesheet
             , Element.layout [] (layout model) ]
    }

colHeader ch = E.el
    [Font.size 18
    ,Font.bold
    ] (E.text ch)

layout model = 
    E.column
        [E.spacing 12
        ,E.centerX]
        [E.el
            [Font.family [ Font.external
                { name = "Roboto"
                , url = "https://fonts.googleapis.com/css?family=Roboto"
                }
            , Font.sansSerif]
            , Font.size 24
            , E.centerX
            ] (E.text "Results for your query")
        , E.column [E.centerX]
            [E.html (viewActiveHits model)
            , EI.slider [E.width <| E.px 720]
                { onChange = UpdateHitFilter
                , label = EI.labelAbove [] (E.text "Slide to filter hits by e-value")
                , min = 1.0
                , max = 100.0
                , value = toFloat model.cutoffIx
                , thumb = EI.defaultThumb
                , step = Just 1.0
                }
            , viewEvalueCutoff model
            ]
        , E.row
            [E.centerX]
            [E.el [E.alignRight] (E.html (viewMap model))
            , E.el [] (E.text (case model.activeSample of
                Nothing -> ""
                Just s -> s.name))]
        , E.html (
                Table.table
                    { options = [ Table.striped, Table.hover ]
                    , thead =  Table.simpleThead
                        [ Table.th [] [ Html.text "Sequence name" ]
                        , Table.th [] [ Html.text "E-value" ]
                        , Table.th [] [ Html.text "Taxon (predicted)" ]
                        ]
                    , tbody = Table.tbody []
                            (List.map (\h ->
                                Table.tr []
                                    [ Table.td [] [ Html.a [href ("http://gmgc.embl.de/search.cgi?search_id="++h.geneID)]
                                                            [Html.text h.geneID ]]
                                    , Table.td [] [ Html.text (niceRound h.evalue) ]
                                    , Table.td [] [ Html.text h.taxon ]
                                    ]) (activeHits model))
                    } ) ]


viewEvalueCutoff : Model -> E.Element Msg
viewEvalueCutoff model =
    let
        mx = Maybe.withDefault 1.0 (
                List.head (List.drop model.cutoffIx model.hits)
                |> Maybe.map (\h -> h.evalue))
    in
        E.text ("Top " ++ (String.fromInt model.cutoffIx) ++ " hits (max e-value: " ++ (niceRound mx) ++ ").")



viewActiveHits : Model -> Html Msg
viewActiveHits model =
    svg 
        [ width "800"
        , height "60"
        ]
        (List.map drawLine (List.range 0 7)
        ++ List.indexedMap (viewActive model) model.hits)

drawLine ix = g []
                [line [stroke "black", strokeWidth "0.1", x1 "0", x2 "720", y1 (String.fromInt (10*ix)), y2 (String.fromInt (10*ix))] []
                ,Svg.text_ [x "720", y (String.fromInt (10*(ix-1))), color "#cccccc", fontSize "9px"] [
                    Svg.text (case ix of
                                                                                0 -> "1.0e-12"
                                                                                1 -> "1.0e-10"
                                                                                2 -> "1.0e-8"
                                                                                3 -> "1.0e-6"
                                                                                4 -> "0.0001"
                                                                                _ -> "0.1"
                                                                                )]
                ]
viewActive model ix h =
    let
        adj x = 1.0 + (logBase 10 x) / 13.0
    in
        rect
            [ x (String.fromInt (7 * ix))
            , y (String.fromFloat (60.0 * adj h.evalue))
            , width "4.6"
            , rx "2"
            , height (String.fromFloat (60.0 * (1.0 - adj h.evalue)))
            , fill (if ix >= model.cutoffIx
                    then "#cccccc"
                    else "#cc3333")
            ] []


mapWidth = 520
mapHeight = 330

gps2coords : Float -> Float -> (Float, Float)
gps2coords lat lon = (mapWidth / 2 + lon / 180.0 * mapWidth / 2, mapHeight / 2 - lat / 90.0 * mapHeight / 2)

viewMap : Model -> Html Msg
viewMap model =
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
        ] ++ List.map viewHitInMap (activeHits model))

viewHitInMap : Hit -> Svg Msg
viewHitInMap h = case h.origin of
    Nothing -> g [] []
    Just s -> viewSample s

viewSample : Sample -> Svg Msg
viewSample s =
        let
            (x,y) = gps2coords s.latitude s.longitude
        in circle [ cx (String.fromFloat x)
                  , cy (String.fromFloat y)
                  , r "6"
                  , Svg.Events.onMouseOver (ActivateSample (Just s))
                  -- , Svg.Events.onMouseOut (ActivateSample Nothing)
                  ] []

getSample : String -> Maybe Sample
getSample n  = List.filter (\s -> n == s.name) gmgcV1samples |> List.head

buildTestHit1 h = { evalue = h.evalue, bitscore = h.bitscore, geneID = h.geneID, taxon = h.taxon, habitat = h.habitat, origin = getSample h.origin }
buildTestHits = List.map buildTestHit1 testHits
