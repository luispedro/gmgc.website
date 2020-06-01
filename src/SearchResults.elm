module SearchResults exposing (..)

import Dict as Dict
import Browser
import Process as Process
import Task as Task

import Html exposing (Html)
import Html.Events exposing (onClick)
import Html.Attributes exposing (href, colspan)

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
import Element.Border as Border
import Element.Background as Background
import Element.Input as EI
import Element.Font as Font

import NiceRound exposing (niceRound)
import Sample exposing (Sample)
import Habitat2Color exposing (habitat2color)
import SampleMap exposing (viewSamplesInMap, viewHabitats)

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


type alias Alignment =
    { seqQ : String
    , match: String
    , seqH : String
    }

type alias Hit =
    { evalue : Float
    , bitscore : Float
    , geneID : String
    , taxon : String
    , habitat : String
    , origin : Maybe Sample
    , isComplete : Bool
    , showAlignment : Bool
    , alignment : Maybe String
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
    | ShowAlignment Int
    | SetAlignment Int

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
    ShowAlignment ix ->
          ( { model | hits = showAlignmentFor ix model.hits }
          , Task.perform (\() -> SetAlignment ix) (Process.sleep 2000 |> Task.andThen (\_ -> Task.succeed ())))
    SetAlignment ix ->
          ( { model | hits = setAlignmentFor ix model.hits }
          , Cmd.none )
    _ -> ( model, Cmd.none )


showAlignmentFor : Int -> List Hit -> List Hit
showAlignmentFor ix = List.indexedMap <| \ix2 h ->
    if ix == ix2
        then { h | showAlignment = True }
        else h

setAlignmentFor : Int -> List Hit -> List Hit
setAlignmentFor ix = List.indexedMap <| \ix2 h ->
    if ix == ix2
        then { h | alignment = Just exampleAlignment }
        else h



view : Model -> Browser.Document Msg
view model =
    { title = "Results for your search query"
    , body = [ CDN.stylesheet
             , Element.layout [] (layout model) ]
    }

layout : Model -> E.Element Msg
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
            [ E.html (Html.h4 [] [Html.text "E-value filter"])
            , E.html (viewActiveHits model)
            , EI.slider [
                E.width <| E.px 720
                , E.behindContent
                    (E.el
                        [ E.width E.fill
                        , E.height (E.px 2)
                        , E.centerY
                        , Background.color (E.rgb 0.5 0.5 0.5)
                        , Border.rounded 2
                        ]
                        Element.none
                    )

                ]
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
            [E.row []
                ( let
                        buttonStyle who =
                            if who == model.onlyComplete then
                                [ Button.info, Button.onClick (SetShowComplete who) ]

                            else
                                [ Button.outlineSecondary, Button.onClick (SetShowComplete who) ]
                    in
                        [ E.el [E.padding 10] (E.html <| Button.button (buttonStyle False) [ Html.text "Show all hits" ] )
                        , E.el [E.padding 10] (E.html <| Button.button (buttonStyle True) [ Html.text "Show only complete ORFs" ] ) ]
                )
            ]
        , E.row
            [E.centerX, E.alignTop]
            [E.el [E.alignRight, E.alignTop] (E.html (viewMap model))
            , E.el [E.alignTop] <| E.html
                (viewHabitats
                    (activeHits model |> List.concatMap (\h -> String.split ", " h.habitat))
                    model.habitatFilter
                    SetHabitatFilter)
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
        , E.html ( -- Element-ui's tables are not as good as Bootstrap's so use Bottstrap here
                Table.table
                    { options = [ Table.striped, Table.hover ]
                    , thead =  Table.simpleThead
                        [ Table.th [] [ Html.text "Sequence name" ]
                        , Table.th [] [ Html.text "E-value" ]
                        , Table.th [] [ Html.text "Taxon (predicted)" ]
                        ]
                    , tbody = Table.tbody []
                            (List.concat <| List.indexedMap (\ix h ->
                                [Table.tr []
                                    [ Table.td [] [ Html.a [href ("http://gmgc.embl.de/search.cgi?search_id="++h.geneID)]
                                                            [Html.text h.geneID ]]
                                    , Table.td [] [ Html.text (niceRound h.evalue ++ " ")
                                                    , Button.button [Button.secondary, Button.light, Button.small, Button.onClick (ShowAlignment ix)] [ Html.text "show alignment" ]
                                                  --, Html.a [href "#", onClick (ShowAlignment ix)] [Html.text " [show alignment]"]
                                                  ]
                                    , Table.td [] [ Html.text h.taxon ]
                                    ]
                                ] ++ (if h.showAlignment
                                        then
                                            [Table.tr [] [ Table.td [Table.cellAttr <| colspan 3] [Html.p []
                                                    (case h.alignment of
                                                            Nothing -> [Html.p []
                                                                                [Spinner.spinner [ Spinner.color Text.primary, Spinner.grow ] []
                                                                                ,Html.text "Computing..."]]
                                                            Just al -> [Html.pre [] [Html.text al] ] -- (al.seqQ ++"\n"++al.match ++ "\n" ++ al.seqH)] ]
                                                    ) ]
                                                    ] ]
                                        else [])

                                    ) (activeHits model))
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
                                                                                5 -> "0.001"
                                                                                6 -> "0.01"
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
                    then (habitat2color h.habitat)
                    else "#dddddd"
            ] []

viewMap : Model -> Html Msg
viewMap model = viewSamplesInMap (List.filterMap (\h -> h.origin) <| activeHits model) (\_ -> NoOp)


getSample : String -> Maybe Sample
getSample n  = List.filter (\s -> n == s.name) gmgcV1samples |> List.head

buildTestHit1 h = { evalue = h.evalue, bitscore = h.bitscore, geneID = h.geneID, taxon = h.taxon, habitat = h.habitat, origin = getSample h.origin, isComplete = h.isComplete, alignment = Nothing, showAlignment = False }

buildTestHits : List Hit
buildTestHits = List.map buildTestHit1 testHits

{-
exampleAlignment =
    { seqQ  =  "AALAMSALMALSJLAJLACAOSIJDAOSIJDALAASKJDASLKJALCEMALWPQRODASLKJALCKMALWPQRODASLKJALCKMALWPQRODASLKJALCKMALWPQROQUPJALSFAASLUFPASUFASFJA"
    , match =  "||||||||||||||||||||||||||||||||.--||||||||||||.||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||.|||||||||||||||"
    , seqH  =  "AALAMSALMALSJLAJLACAOSIJDAOSIJDAI  SKJDASLKJALCKMALWPQRODASLKJALCKMALWPQRODASLKJALCKMALWPQRODASLKJALCKMALWPQROQUPJALSFVASLUFPASUFASFJA"
    }
-}
exampleAlignment : String
exampleAlignment = """
Identity: 97.4%

AALAMSALMALSJLAJLACAOSIJDAOSIJDALAASKJDASLKJALCEMALWPQRODASLKJALCKMALWPQRODASLKJ
||||||||||||||||||||||||||||||||.  ||||||||||||.||||||||||||||||||||||||||||||||
AALAMSALMALSJLAJLACAOSIJDAOSIJDAI--SKJDASLKJALCKMALWPQRODASLKJALCKMALWPQRODASLKJ



ALCCKMALWPQRODASLKJALCKMALWPQROQUPJALSFAASLUFPASUFASFJA
||| |||||||||||||||||||||||||||||||||||.|||||||||||||||
ALC-KMALWPQRODASLKJALCKMALWPQROQUPJALSFVASLUFPASUFASFJA
"""
