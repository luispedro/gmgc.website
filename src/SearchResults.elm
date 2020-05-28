module SearchResults exposing (..)

import Dict as Dict
import Browser
import Html exposing (Html)
import Html.Attributes exposing (href)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Popover as Popover
import Bootstrap.Text as Text
import Bootstrap.Table as Table
import Bootstrap.Spinner as Spinner

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
    , isComplete : Bool
    }

type alias Model =
    { hits : List Hit
    , cutoffIx : Int
    , onlyComplete : Bool
    , habitatFilter : Maybe String
    , activeSample : Maybe Sample
    }

activeHits : Model -> List Hit
activeHits model =
    let
        filterComplete hits =
            if model.onlyComplete
                then List.filter (\h -> h.isComplete) hits
                else hits
        filterHabitats hits = case model.habitatFilter of
            Nothing -> hits
            Just hab -> List.filter (\h -> List.member hab (String.split ", " h.habitat)) hits
    in
        List.take model.cutoffIx model.hits
            |> filterComplete
            |> filterHabitats

init : () -> ( Model, Cmd Msg )
init () =
  ( { hits = buildTestHits
    , cutoffIx = 50
    , onlyComplete = False
    , habitatFilter = Nothing
    , activeSample = Nothing
    }
  , Cmd.none
  )


type Msg
    = NoOp
    | UpdateHitFilter Float
    | SetShowComplete Bool
    | SetHabitatFilter (Maybe String)
    | ActivateSample (Maybe Sample)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
    UpdateHitFilter nv ->
          ( { model | cutoffIx = round nv }
          , Cmd.none )
    SetHabitatFilter hf ->
          ( { model | habitatFilter = hf }
          , Cmd.none )
    SetShowComplete sc  ->
          ( { model | onlyComplete = sc }
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
        , E.column [E.centerX]
            [E.html <|
                    let
                        buttonStyle who =
                            if who == model.onlyComplete then
                                [ Button.info, Button.onClick (SetShowComplete who) ]

                            else
                                [ Button.outlineSecondary, Button.onClick (SetShowComplete who) ]
                    in Grid.simpleRow
                        [ Grid.col [] [ Button.button (buttonStyle False) [ Html.text "Show all hits" ] ]
                        , Grid.col [] [ Button.button (buttonStyle True) [ Html.text "Show only complete ORFs" ] ]
                        ]
            ]
        , E.row
            [E.centerX, E.alignTop]
            [E.el [E.alignRight, E.alignTop] (E.html (viewMap model))
            ,displayHabitats model
            ]
            {-, E.el [] (E.text (case model.activeSample of
                Nothing -> ""
           Just s -> s.name))] -}
        , E.row
            []
            [E.html <|
                Grid.simpleRow
                (case model.habitatFilter of
                Nothing -> [ Grid.col [] [ Html.h4 [] [Html.text "Habitat filtering"]
                                          , Html.p [] [Html.text "Click on the habitat names above to filter for genes matching a particular habitat."]
                                          ]
                            ]
                Just hab ->
                        [ Grid.col [] [ Html.h4 [] [Html.text ("Habitat filtering: "++hab)]
                                        ,Html.p []
                                        [Html.text "Only including unigenes annotated as "
                                        ,Html.i [] [Html.text hab]
                                        ,Html.text " (but note that unigenes may be annotated to multiple habitats)." ]
                                        , Button.button [Button.warning, Button.onClick (SetHabitatFilter Nothing)] [ Html.text "Clear Habitat filter" ] ]
                        ])
            ]
        , E.row []
            [E.text "Download all as "
            ,E.html <| Html.a [href ("#")] [ Html.text "Protein" ]
            ,E.text " or as "
            ,E.html <| Html.a [href ("#")] [ Html.text "DNA" ]
            ,E.text <| " (" ++ (activeHits model |> List.length |> String.fromInt) ++ " hits)."
            ]
        , E.html (Html.hr [] [])
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
        nactive = List.length (activeHits model)
    in
        E.text ("Top " ++ (String.fromInt model.cutoffIx)
            ++ (if nactive /= model.cutoffIx
                    then " ("++(String.fromInt <| List.length (activeHits model)) ++ " after habitat & completeness filtering)"
                    else "")
            ++ " hits (max e-value: " ++ (niceRound mx) ++ ").")



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
        isHabitatActive = case model.habitatFilter of
            Just hab -> List.member hab (String.split ", " h.habitat)
            Nothing -> True
        isActive = (ix <= model.cutoffIx) && isHabitatActive && (h.isComplete || not model.onlyComplete)
    in
        rect
            [ x (String.fromInt (7 * ix))
            , y (String.fromFloat (60.0 * adj h.evalue))
            , width "4.6"
            , rx "2"
            , height (String.fromFloat (60.0 * (1.0 - adj h.evalue)))
            , fill <| if isActive
                    then (biome2color h.habitat)
                    else "#dddddd"
            ] []


mapWidth = 520
mapHeight = 330

gps2coords : Float -> Float -> (Float, Float)
gps2coords lat lon = (mapWidth / 2 + lon / 180.0 * mapWidth / 2, mapHeight / 2 - lat / 90.0 * mapHeight / 2)


countHabitats : List String -> Dict.Dict String Int
countHabitats hs =
    let
        add1 a = Just <| 1 + Maybe.withDefault 0 a
    in List.foldl (\c h -> Dict.update c add1 h) Dict.empty hs

displayHabitats model =
    let
        habitatCounts = activeHits model |> List.concatMap (\h -> String.split ", " h.habitat) |> countHabitats
        filtered = Dict.toList habitatCounts |>
            List.sortBy (\(h,c) -> 0 - c) |>
            List.filter (\(h,_) -> h /= "-") |>
            List.take 6
    in E.el [E.alignTop] <| E.html
        (svg
            [ width "500"
            , height (String.fromInt <| 60 + 30 * (List.length filtered))
            ]
            (filtered |> List.indexedMap (\ix (h,c) ->
                let
                    isActive = Maybe.withDefault "X" model.habitatFilter == h
                in
                    (Svg.g [ Svg.Events.onClick (SetHabitatFilter (if isActive then Nothing else (Just h))) ]
                        [Svg.rect
                            [x "0"
                            , y (String.fromInt <| 60 + 30 * ix)
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
                            , y (String.fromInt <| 60 + 30 * (1+ix) - 15)
                            , fontSize "12px"]
                            [ Svg.text h ]
                        ,rect
                            [ x "140"
                            , y (String.fromInt (60 + 30 * ix))
                            , width (String.fromInt <| c * 10)
                            , rx "2"
                            , height (String.fromInt 20)
                            , fill (biome2color h)
                            ] []
                        ,Svg.text_
                            [ x "120"
                            , y (String.fromInt <| 60 + 30 * (1+ix) - 15)
                            , color "#ffffff"
                            , fontSize "14px"]
                            [ Svg.text (String.fromInt c) ]
                        ]
                        ))))

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
                  , fill (biome2color s.habitat)
                  -- , Svg.Events.onMouseOver (ActivateSample (Just s))
                  -- , Svg.Events.onMouseOut (ActivateSample Nothing)
                  ] []

getSample : String -> Maybe Sample
getSample n  = List.filter (\s -> n == s.name) gmgcV1samples |> List.head

buildTestHit1 h = { evalue = h.evalue, bitscore = h.bitscore, geneID = h.geneID, taxon = h.taxon, habitat = h.habitat, origin = getSample h.origin, isComplete = h.isComplete }
buildTestHits = List.map buildTestHit1 testHits

biome2color : String -> String
biome2color b = Dict.get b biome2colorTable |> Maybe.withDefault "#666666"

biome2colorTable : Dict.Dict String String
biome2colorTable = Dict.fromList [
    ("cat gut", "#553105ff"),
    ("dog gut", "#93570fff"),
    ("human gut", "#dd861dff"),
    ("pig gut" , "#fec008ff"),
    ("mouse gut", "#ba1902ff"),
    ("human nose", "#792597ff"),
    ("human oral", "#82bb47ff"),
    ("human skin", "#c797d0ff"),
    ("human vagina", "#7c9dd9ff"),
    ("marine", "#0522bbff"),
    ("soil", "#028f00ff"),
    ("built-environment", "#000000e6"),
    ("freshwater", "#0076d5ff"),
    ("wastewater", "#767171ff"),


    ("other human", "#792597ff"),
    ("mammal gut", "#966906fd")
    ]
