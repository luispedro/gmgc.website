module GenomeBinSearchResult exposing (..)

import Dict as Dict
import Browser
import Html exposing (Html)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)

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

import Bootstrap.CDN as CDN
import Bootstrap.Table as Table

import NiceRound exposing (niceRound)
import Sample exposing (Sample)
import Habitat2Color exposing (habitat2color)
import SampleMap exposing (viewSamplesInMap, viewHabitats)

import GMGCv1samples exposing (gmgcV1samples)

main : Program () Model Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }


type Quality = HighQ | MedQ | LowQ

type alias GenomeBin =
    { gmbcID : String
    , quality : Quality
    , origin : Sample
    }

type alias Model =
    { bins : List GenomeBin
    , keepHigh : Bool
    , keepMed : Bool
    , keepLow : Bool
    , habitatFilter : Maybe String
    , sampleFilter : Maybe Sample
    }

activeBins : Model -> List GenomeBin
activeBins model =
    let
        isActiveQ b = case b.quality of
            HighQ -> model.keepHigh
            MedQ -> model.keepMed
            LowQ -> model.keepLow
        isActiveH b = case model.habitatFilter of
            Nothing -> True
            Just h -> b.origin.habitat == h
        isActiveS b = case model.sampleFilter of
            Nothing -> True
            Just s -> (abs (b.origin.latitude - s.latitude) < 4) && (abs (b.origin.longitude - s.longitude) < 4)
        isActive b = isActiveQ b && isActiveH b && isActiveS b
    in
        List.filter isActive model.bins

init : () -> ( Model, Cmd Msg )
init () =
  ( { bins = buildTestBins
    , keepHigh = True
    , keepMed = True
    , keepLow = True
    , habitatFilter = Nothing
    , sampleFilter = Nothing
  }
  , Cmd.none
  )


type Msg
    = NoOp
    | SetHighFilter Bool
    | SetMedFilter Bool
    | SetLowFilter Bool
    | SetHabitatFilter (Maybe String)
    | SetSampleFilter (Maybe Sample)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
    NoOp -> ( model, Cmd.none )
    SetHighFilter f -> ( { model | keepHigh = f }, Cmd.none )
    SetMedFilter f -> ( { model | keepMed = f }, Cmd.none )
    SetLowFilter f -> ( { model | keepLow = f }, Cmd.none )
    SetHabitatFilter hf ->
          ( { model | habitatFilter = hf }
          , Cmd.none )
    SetSampleFilter sf ->
          ( { model | sampleFilter = sf }
          , Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Results for your query"
    , body = [ CDN.stylesheet
             , Element.layout [] (layout model) ]
    }


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
            (let
                    buttonStyle msg state =
                        if state then
                            [ Button.info, Button.onClick (msg <| not state) ]
                        else
                            [ Button.outlineSecondary, Button.onClick (msg <| not state) ]
                in
                    [ E.el [] (E.text "Use the buttons to filter bins")
                    , E.row [E.spacing 12]
                        [ E.html <| Button.button (buttonStyle SetHighFilter model.keepHigh) [ Html.text <| "High quality" ]
                        , E.html <| Button.button (buttonStyle SetMedFilter model.keepMed) [ Html.text <| "Medium quality" ]
                        , E.html <| Button.button (buttonStyle SetLowFilter model.keepLow) [ Html.text <| "Low quality" ]
                        ]
                    ]

            )
        , E.row
            [E.centerX, E.alignTop]
            [E.el [E.alignRight, E.alignTop] (E.html (viewMap model))
            , E.el [E.alignTop] <| E.html
                (viewHabitats
                    (activeBins model |> List.map (\b -> b.origin.habitat))
                    model.habitatFilter
                    SetHabitatFilter)
            ]
        , case model.sampleFilter of
            Nothing -> E.el [] (E.text "")
            Just s -> E.row []
                        [E.html <|
                            Grid.simpleRow
                                [ Grid.col []
                                    [Html.h4 [] [Html.text "Geographic filter active"]
                                    ,Html.p [] [Html.a [onClick (SetSampleFilter Nothing), href "#"]  [Html.text "clear geographic filter"]]
                                    ]
                                ]
                        ]
        , E.row
            []
            [E.html <|
                Grid.simpleRow
                (case model.habitatFilter of
                Nothing -> [ Grid.col [] [ Html.h4 [] [Html.text "Habitat filtering"]
                                          , Html.p [] [Html.text "Click on the habitat names above to filter for unigenes matching a particular habitat."]
                                          ]
                            ]
                Just hab ->
                        [ Grid.col [] [ Html.h4 [] [Html.text ("Habitat filtering: "++hab)]
                                        ,Html.p []
                                        [Html.text "Only including bins assembled from "
                                        ,Html.i [] [Html.text hab]
                                        ,Html.text "." ]
                                        , Button.button [Button.warning, Button.onClick (SetHabitatFilter Nothing)] [ Html.text "Clear Habitat filter" ] ]
                        ])
            ]
        , E.row []
            [E.text "Download all as "
            ,E.html <| Html.a [href ("#")] [ Html.text "FASTA File" ]
            ,E.text <| " (" ++ (activeBins model |> List.length |> String.fromInt) ++ " hits)."
            ]
        , E.html (Html.hr [] [])
        , E.html ( -- Boostrap's tables are better
                Table.table
                    { options = [ Table.striped, Table.hover ]
                    , thead =  Table.simpleThead
                        [ Table.th [] [ Html.text "Bin ID" ]
                        , Table.th [] [ Html.text "Quality" ]
                        ]
                    , tbody = Table.tbody []
                            (List.map (\h ->
                                Table.tr []
                                    [ Table.td [] [ Html.a [href "#"]
                                                            [Html.text h.gmbcID ]]
                                    , Table.td [] [ Html.text (quality h.quality) ]
                                    ]) (activeBins model))
                    } ) ]


quality : Quality -> String
quality q = case q of
    HighQ -> "high-quality"
    MedQ -> "medium-quality"
    LowQ -> "low-quality"

viewMap : Model -> Html Msg
viewMap model = viewSamplesInMap (List.map (\h -> h.origin) <| activeBins model) SetSampleFilter

getSample : String -> Maybe Sample
getSample n  = List.filter (\s -> n == s.name) gmgcV1samples |> List.head

buildTestBins = List.map buildTestBins1 testBins

buildTestBins1 h =
        { gmbcID = h.gmbcID, quality = h.quality, origin = Maybe.withDefault defaultSample (getSample h.origin) }
defaultSample = 
    { name = "SAMEA3663120", latitude = 55.21744, longitude = 10.661669999999999, habitat = "pig gut" }

testBins =
    [ { gmbcID = "GMBC10.000_458", quality = HighQ, origin = "SAMN05826563" }
    , { gmbcID = "GMBC10.000_459", quality = HighQ, origin = "SAMN04436798" }
    , { gmbcID = "GMBC10.000_460", quality = HighQ, origin = "SAMN04436776" }
    , { gmbcID = "GMBC10.000_461", quality = HighQ, origin = "SAMN04436773" }
    , { gmbcID = "GMBC10.000_462", quality = HighQ, origin = "SAMN04436772" }
    , { gmbcID = "GMBC10.000_463", quality = HighQ, origin = "SAMN04436734" }
    , { gmbcID = "GMBC10.000_465", quality = HighQ, origin = "SAMN01780001" }
    , { gmbcID = "GMBC10.000_466", quality = HighQ, origin = "SAMN01779904" }
    , { gmbcID = "GMBC10.000_468", quality = HighQ, origin = "SAMEA2467018" }
    , { gmbcID = "GMBC10.000_480", quality = HighQ, origin = "SAMN05827210" }
    , { gmbcID = "GMBC10.000_481", quality = HighQ, origin = "SAMN05827115" }
    , { gmbcID = "GMBC10.000_594", quality = HighQ, origin = "SAMN04436752" }
    , { gmbcID = "GMBC10.000_635", quality = HighQ, origin = "SAMEA2737655" }
    , { gmbcID = "GMBC10.000_646", quality = HighQ, origin = "SAMN04436696" }
    , { gmbcID = "GMBC10.000_656", quality = HighQ, origin = "SAMN04436692" }
    , { gmbcID = "GMBC10.000_689", quality = HighQ, origin = "SAMN04436743" }
    , { gmbcID = "GMBC10.000_690", quality = HighQ, origin = "SAMN04436737" }
    , { gmbcID = "GMBC10.000_747", quality = HighQ, origin = "SAMEA2580039" }
    , { gmbcID = "GMBC10.000_761", quality = HighQ, origin = "SAMEA3449238" }
    , { gmbcID = "GMBC10.000_771", quality = HighQ, origin = "SAMN04436775" }
    , { gmbcID = "GMBC10.000_772", quality = HighQ, origin = "SAMN04436771" }
    , { gmbcID = "GMBC10.000_773", quality = HighQ, origin = "SAMN04436768" }
    , { gmbcID = "GMBC10.000_774", quality = HighQ, origin = "SAMN04436690" }
    , { gmbcID = "GMBC10.000_775", quality = HighQ, origin = "SAMEA3449217" }
    , { gmbcID = "GMBC10.000_776", quality = HighQ, origin = "SAMEA1906563" }
    , { gmbcID = "GMBC10.000_900", quality = HighQ, origin = "SAMN04436935" }
    , { gmbcID = "GMBC10.001_292", quality = HighQ, origin = "SAMN04436769" }
    , { gmbcID = "GMBC10.001_334", quality = HighQ, origin = "SAMEA2580083" }
    , { gmbcID = "GMBC10.001_336", quality = HighQ, origin = "SAMEA3541593" }
    , { gmbcID = "GMBC10.001_382", quality = HighQ, origin = "SAMN04436928" }
    , { gmbcID = "GMBC10.001_586", quality = HighQ, origin = "SAMN04436697" }
    , { gmbcID = "GMBC10.001_590", quality = HighQ, origin = "SAMN04436736" }
    , { gmbcID = "GMBC10.001_591", quality = HighQ, origin = "SAMN04436733" }
    , { gmbcID = "GMBC10.001_592", quality = HighQ, origin = "SAMN04436590" }
    , { gmbcID = "GMBC10.001_595", quality = HighQ, origin = "SAMEA3951783" }
    , { gmbcID = "GMBC10.001_596", quality = HighQ, origin = "SAMEA3449379" }
    , { gmbcID = "GMBC10.001_597", quality = HighQ, origin = "SAMEA3136740" }
    , { gmbcID = "GMBC10.001_633", quality = HighQ, origin = "SAMN03270951" }
    , { gmbcID = "GMBC10.001_818", quality = HighQ, origin = "SAMN05826950" }
    , { gmbcID = "GMBC10.001_820", quality = HighQ, origin = "SAMN04436786" }
    , { gmbcID = "GMBC10.001_920", quality = HighQ, origin = "SAMN01774051" }
    , { gmbcID = "GMBC10.001_937", quality = HighQ, origin = "SAMEA3879584" }
    , { gmbcID = "GMBC10.001_939", quality = HighQ, origin = "SAMEA2580194" }
    , { gmbcID = "GMBC10.001_968", quality = HighQ, origin = "SAMEA2580228" }
    , { gmbcID = "GMBC10.001_984", quality = HighQ, origin = "SAMN04436694" }
    , { gmbcID = "GMBC10.001_989", quality = HighQ, origin = "SAMEA4378269" }
    , { gmbcID = "GMBC10.001_999", quality = HighQ, origin = "SAMEA2580087" }
    , { gmbcID = "GMBC10.002_055", quality = HighQ, origin = "SAMN05545071" }
    , { gmbcID = "GMBC10.002_056", quality = HighQ, origin = "SAMN01779902" }
    , { gmbcID = "GMBC10.002_057", quality = HighQ, origin = "SAMEA3879545" }
    , { gmbcID = "GMBC10.002_070", quality = HighQ, origin = "SAMEA3541555" }
    , { gmbcID = "GMBC10.002_104", quality = HighQ, origin = "SAMEA4378311" }
    , { gmbcID = "GMBC10.002_111", quality = HighQ, origin = "SAMEA3664945" }
    , { gmbcID = "GMBC10.002_112", quality = HighQ, origin = "SAMEA3541492" }
    , { gmbcID = "GMBC10.002_116", quality = HighQ, origin = "SAMEA2580151" }
    , { gmbcID = "GMBC10.002_118", quality = HighQ, origin = "SAMEA2579953" }
    , { gmbcID = "GMBC10.002_191", quality = HighQ, origin = "SAMEA2466940" }
    , { gmbcID = "GMBC10.002_241", quality = HighQ, origin = "SAMN04436691" }
    , { gmbcID = "GMBC10.002_286", quality = HighQ, origin = "SAMEA2582047" }
    , { gmbcID = "GMBC10.002_340", quality = HighQ, origin = "SAMN01779893" }
    , { gmbcID = "GMBC10.002_359", quality = HighQ, origin = "SAMEA2580123" }
    , { gmbcID = "GMBC10.002_917", quality = HighQ, origin = "SAMEA2580100" }
    , { gmbcID = "GMBC10.002_958", quality = HighQ, origin = "SAMN04436934" }
    , { gmbcID = "GMBC10.002_962", quality = HighQ, origin = "SAMN04436698" }
    , { gmbcID = "GMBC10.002_963", quality = HighQ, origin = "SAMN04436689" }
    , { gmbcID = "GMBC10.002_978", quality = HighQ, origin = "SAMN04436693" }
    , { gmbcID = "GMBC10.002_999", quality = HighQ, origin = "SAMEA2580139" }
    , { gmbcID = "GMBC10.003_000", quality = HighQ, origin = "SAMEA2466989" }
    , { gmbcID = "GMBC10.003_078", quality = HighQ, origin = "SAMN04261146" }
    , { gmbcID = "GMBC10.003_080", quality = HighQ, origin = "SAMEA3951707" }
    , { gmbcID = "GMBC10.003_091", quality = HighQ, origin = "SAMEA4378236" }
    , { gmbcID = "GMBC10.003_096", quality = HighQ, origin = "SAMEA3664571" }
    , { gmbcID = "GMBC10.003_102", quality = HighQ, origin = "SAMEA2582085" }
    , { gmbcID = "GMBC10.003_103", quality = HighQ, origin = "SAMEA2579987" }
    , { gmbcID = "GMBC10.003_180", quality = HighQ, origin = "SAMN05827165" }
    , { gmbcID = "GMBC10.003_187", quality = HighQ, origin = "SAMEA3541475" }
    , { gmbcID = "GMBC10.003_203", quality = HighQ, origin = "SAMEA2580046" }
    , { gmbcID = "GMBC10.003_226", quality = HighQ, origin = "SAMEA2582030" }
    , { gmbcID = "GMBC10.003_235", quality = HighQ, origin = "SAMEA2582048" }
    , { gmbcID = "GMBC10.003_236", quality = HighQ, origin = "SAMEA2579945" }
    , { gmbcID = "GMBC10.003_267", quality = HighQ, origin = "SAMEA2582118" }
    , { gmbcID = "GMBC10.003_268", quality = HighQ, origin = "SAMEA2580179" }
    , { gmbcID = "GMBC10.003_269", quality = HighQ, origin = "SAMEA2580099" }
    , { gmbcID = "GMBC10.003_338", quality = HighQ, origin = "SAMEA2579999" }
    , { gmbcID = "GMBC10.003_358", quality = HighQ, origin = "SAMN04261271" }
    , { gmbcID = "GMBC10.003_363", quality = HighQ, origin = "SAMN01779886" }
    , { gmbcID = "GMBC10.003_396", quality = HighQ, origin = "SAMEA2737774" }
    , { gmbcID = "GMBC10.003_483", quality = HighQ, origin = "SAMN05545053" }
    , { gmbcID = "GMBC10.003_487", quality = HighQ, origin = "SAMEA3951606" }
    , { gmbcID = "GMBC10.003_488", quality = HighQ, origin = "SAMEA3449231" }
    , { gmbcID = "GMBC10.003_491", quality = HighQ, origin = "SAMEA3541526" }
    , { gmbcID = "GMBC10.003_559", quality = HighQ, origin = "SAMEA4378275" }
    , { gmbcID = "GMBC10.003_591", quality = HighQ, origin = "SAMEA2580258" }
    , { gmbcID = "GMBC10.003_614", quality = HighQ, origin = "SAMEA3541497" }
    , { gmbcID = "GMBC10.003_668", quality = HighQ, origin = "SAMN04436800" }
    , { gmbcID = "GMBC10.003_706", quality = HighQ, origin = "SAMN04436607" }
    , { gmbcID = "GMBC10.003_799", quality = HighQ, origin = "SAMEA3136659" }
    , { gmbcID = "GMBC10.003_837", quality = HighQ, origin = "SAMEA2580114" }
    , { gmbcID = "GMBC10.003_939", quality = HighQ, origin = "SAMN04436897" }
    , { gmbcID = "GMBC10.003_940", quality = HighQ, origin = "SAMN04436833" }
    , { gmbcID = "GMBC10.003_941", quality = HighQ, origin = "SAMN04436750" }
    , { gmbcID = "GMBC10.003_945", quality = HighQ, origin = "SAMN00696722" }
    , { gmbcID = "GMBC10.003_949", quality = HighQ, origin = "SAMEA2580071" }
    , { gmbcID = "GMBC10.003_960", quality = HighQ, origin = "SAMN00990321" }
    , { gmbcID = "GMBC10.004_289", quality = HighQ, origin = "SAMN05545069" }
    , { gmbcID = "GMBC10.004_290", quality = HighQ, origin = "SAMN03271140" }
    , { gmbcID = "GMBC10.004_300", quality = HighQ, origin = "SAMN05827258" }
    , { gmbcID = "GMBC10.004_486", quality = HighQ, origin = "SAMEA4378262" }
    , { gmbcID = "GMBC10.004_516", quality = HighQ, origin = "SAMEA3136632" }
    , { gmbcID = "GMBC10.004_531", quality = HighQ, origin = "SAMN03025399" }
    , { gmbcID = "GMBC10.004_604", quality = HighQ, origin = "SAMN04436927" }
    , { gmbcID = "GMBC10.004_612", quality = HighQ, origin = "SAMN01773444" }
    , { gmbcID = "GMBC10.004_665", quality = HighQ, origin = "SAMEA2580171" }
    , { gmbcID = "GMBC10.004_667", quality = HighQ, origin = "SAMN04436788" }
    , { gmbcID = "GMBC10.004_668", quality = HighQ, origin = "SAMN04436785" }
    , { gmbcID = "GMBC10.004_706", quality = HighQ, origin = "SAMN02676208" }
    , { gmbcID = "GMBC10.004_717", quality = HighQ, origin = "SAMN04436926" }
    , { gmbcID = "GMBC10.004_718", quality = HighQ, origin = "SAMN04436728" }
    , { gmbcID = "GMBC10.004_725", quality = HighQ, origin = "SAMEA3449419" }
    , { gmbcID = "GMBC10.004_760", quality = HighQ, origin = "SAMN04436787" }
    , { gmbcID = "GMBC10.004_864", quality = HighQ, origin = "SAMN01779924" }
    , { gmbcID = "GMBC10.004_865", quality = HighQ, origin = "SAMEA3449371" }
    , { gmbcID = "GMBC10.004_874", quality = HighQ, origin = "SAMN05545097" }
    , { gmbcID = "GMBC10.004_875", quality = HighQ, origin = "SAMN05545085" }
    , { gmbcID = "GMBC10.004_876", quality = HighQ, origin = "SAMN05545021" }
    , { gmbcID = "GMBC10.004_877", quality = HighQ, origin = "SAMN05544997" }
    , { gmbcID = "GMBC10.004_878", quality = HighQ, origin = "SAMN05544993" }
    , { gmbcID = "GMBC10.004_879", quality = HighQ, origin = "SAMN04436751" }
    , { gmbcID = "GMBC10.004_880", quality = HighQ, origin = "SAMN04436695" }
    , { gmbcID = "GMBC10.004_881", quality = HighQ, origin = "SAMN04436608" }
    , { gmbcID = "GMBC10.004_882", quality = HighQ, origin = "SAMN04261290" }
    , { gmbcID = "GMBC10.004_890", quality = HighQ, origin = "SAMEA3449256" }
    , { gmbcID = "GMBC10.004_894", quality = HighQ, origin = "SAMEA2580058" }
    , { gmbcID = "GMBC10.004_900", quality = HighQ, origin = "SAMEA2737715" }
    , { gmbcID = "GMBC10.004_940", quality = HighQ, origin = "SAMEA3879572" }
    , { gmbcID = "GMBC10.004_944", quality = HighQ, origin = "SAMEA3449358" }
    , { gmbcID = "GMBC10.004_950", quality = HighQ, origin = "SAMN05545055" }
    , { gmbcID = "GMBC10.004_953", quality = HighQ, origin = "SAMEA3449194" }
    , { gmbcID = "GMBC10.004_960", quality = HighQ, origin = "SAMEA2580060" }
    , { gmbcID = "GMBC10.004_961", quality = HighQ, origin = "SAMEA2580055" }
    , { gmbcID = "GMBC10.004_962", quality = HighQ, origin = "SAMEA2579932" }
    , { gmbcID = "GMBC10.004_986", quality = HighQ, origin = "SAMEA3449274" }
    , { gmbcID = "GMBC10.004_989", quality = HighQ, origin = "SAMEA2581883" }
    , { gmbcID = "GMBC10.004_990", quality = HighQ, origin = "SAMEA2580301" }
    , { gmbcID = "GMBC10.005_119", quality = HighQ, origin = "SAMEA3951697" }
    , { gmbcID = "GMBC10.005_127", quality = HighQ, origin = "SAMEA3708698" }
    , { gmbcID = "GMBC10.005_192", quality = HighQ, origin = "SAMEA2580182" }
    , { gmbcID = "GMBC10.005_231", quality = HighQ, origin = "SAMN02333922" }
    , { gmbcID = "GMBC10.005_263", quality = HighQ, origin = "SAMN05545054" }
    , { gmbcID = "GMBC10.005_264", quality = HighQ, origin = "SAMEA3449196" }
    , { gmbcID = "GMBC10.005_275", quality = HighQ, origin = "SAMEA2737652" }
    , { gmbcID = "GMBC10.005_299", quality = HighQ, origin = "SAMN05826487" }
    , { gmbcID = "GMBC10.005_332", quality = HighQ, origin = "SAMEA3449261" }
    , { gmbcID = "GMBC10.005_334", quality = HighQ, origin = "SAMEA2580288" }
    , { gmbcID = "GMBC10.005_346", quality = HighQ, origin = "SAMEA2582098" }
    , { gmbcID = "GMBC10.005_347", quality = HighQ, origin = "SAMEA2579919" }
    , { gmbcID = "GMBC10.005_370", quality = HighQ, origin = "SAMEA2737661" }
    , { gmbcID = "GMBC10.005_415", quality = HighQ, origin = "SAMEA2580007" }
    , { gmbcID = "GMBC10.005_452", quality = HighQ, origin = "SAMEA2466944" }
    , { gmbcID = "GMBC10.005_467", quality = HighQ, origin = "SAMEA2580149" }
    , { gmbcID = "GMBC10.005_470", quality = HighQ, origin = "SAMN05826913" }
    , { gmbcID = "GMBC10.005_486", quality = HighQ, origin = "SAMN05827176" }
    , { gmbcID = "GMBC10.005_513", quality = HighQ, origin = "SAMN04436865" }
    , { gmbcID = "GMBC10.005_531", quality = HighQ, origin = "SAMN04436730" }
    , { gmbcID = "GMBC10.005_533", quality = HighQ, origin = "SAMN04261161" }
    , { gmbcID = "GMBC10.005_558", quality = HighQ, origin = "SAMN04436932" }
    , { gmbcID = "GMBC10.005_566", quality = HighQ, origin = "SAMEA3449237" }
    , { gmbcID = "GMBC10.005_601", quality = HighQ, origin = "SAMN05826916" }
    , { gmbcID = "GMBC10.005_612", quality = HighQ, origin = "SAMEA2579962" }
    , { gmbcID = "GMBC10.005_984", quality = HighQ, origin = "SAMEA3541568" }
    , { gmbcID = "GMBC10.005_989", quality = HighQ, origin = "SAMEA2580076" }
    , { gmbcID = "GMBC10.006_033", quality = HighQ, origin = "SAMN04261270" }
    , { gmbcID = "GMBC10.006_088", quality = HighQ, origin = "SAMEA2580263" }
    , { gmbcID = "GMBC10.006_197", quality = HighQ, origin = "SAMEA2737835" }
    , { gmbcID = "GMBC10.006_226", quality = HighQ, origin = "SAMEA2580035" }
    , { gmbcID = "GMBC10.006_228", quality = HighQ, origin = "SAMN05545012" }
    , { gmbcID = "GMBC10.006_273", quality = HighQ, origin = "SAMN04436778" }
    , { gmbcID = "GMBC10.006_274", quality = HighQ, origin = "SAMN04436748" }
    , { gmbcID = "GMBC10.006_277", quality = HighQ, origin = "SAMN03271010" }
    , { gmbcID = "GMBC10.006_283", quality = HighQ, origin = "SAMEA3449302" }
    , { gmbcID = "GMBC10.006_289", quality = HighQ, origin = "SAMN04436639" }
    , { gmbcID = "GMBC10.006_445", quality = HighQ, origin = "SAMEA3541486" }
    , { gmbcID = "GMBC10.006_464", quality = HighQ, origin = "SAMN05545099" }
    , { gmbcID = "GMBC10.006_465", quality = HighQ, origin = "SAMN05545096" }
    , { gmbcID = "GMBC10.006_466", quality = HighQ, origin = "SAMN05545095" }
    , { gmbcID = "GMBC10.006_467", quality = HighQ, origin = "SAMN05545093" }
    , { gmbcID = "GMBC10.006_468", quality = HighQ, origin = "SAMN05545030" }
    , { gmbcID = "GMBC10.006_469", quality = HighQ, origin = "SAMN05545022" }
    , { gmbcID = "GMBC10.006_470", quality = HighQ, origin = "SAMN05545011" }
    , { gmbcID = "GMBC10.006_471", quality = HighQ, origin = "SAMN05545006" }
    , { gmbcID = "GMBC10.006_472", quality = HighQ, origin = "SAMN05545002" }
    , { gmbcID = "GMBC10.006_473", quality = HighQ, origin = "SAMN05545001" }
    , { gmbcID = "GMBC10.006_474", quality = HighQ, origin = "SAMN05544998" }
    , { gmbcID = "GMBC10.006_475", quality = HighQ, origin = "SAMN05544996" }
    , { gmbcID = "GMBC10.006_476", quality = HighQ, origin = "SAMN05544995" }
    , { gmbcID = "GMBC10.006_553", quality = HighQ, origin = "SAMEA3708597" }
    , { gmbcID = "GMBC10.006_562", quality = HighQ, origin = "SAMEA3708719" }
    , { gmbcID = "GMBC10.006_592", quality = HighQ, origin = "SAMN05545019" }
    , { gmbcID = "GMBC10.006_593", quality = HighQ, origin = "SAMN05545016" }
    , { gmbcID = "GMBC10.006_624", quality = HighQ, origin = "SAMEA3136768" }
    , { gmbcID = "GMBC10.006_643", quality = HighQ, origin = "SAMN00991598" }
    , { gmbcID = "GMBC10.006_696", quality = HighQ, origin = "SAMEA2580136" }
    , { gmbcID = "GMBC10.006_766", quality = HighQ, origin = "SAMEA2580240" }
    , { gmbcID = "GMBC10.006_798", quality = HighQ, origin = "SAMEA3136628" }
    , { gmbcID = "GMBC10.006_817", quality = HighQ, origin = "SAMEA4378243" }
    , { gmbcID = "GMBC10.006_848", quality = HighQ, origin = "SAMEA2580296" }
    , { gmbcID = "GMBC10.006_864", quality = HighQ, origin = "SAMN05545013" }
    , { gmbcID = "GMBC10.006_881", quality = HighQ, origin = "SAMN01780015" }
    , { gmbcID = "GMBC10.006_994", quality = HighQ, origin = "SAMN05827127" }
    , { gmbcID = "GMBC10.007_030", quality = HighQ, origin = "SAMEA3664607" }
    , { gmbcID = "GMBC10.007_048", quality = HighQ, origin = "SAMEA3951800" }
    , { gmbcID = "GMBC10.007_057", quality = HighQ, origin = "SAMN01919664" }
    , { gmbcID = "GMBC10.007_156", quality = HighQ, origin = "SAMEA2338803" }
    , { gmbcID = "GMBC10.007_174", quality = HighQ, origin = "SAMEA3708532" }
    , { gmbcID = "GMBC10.007_225", quality = HighQ, origin = "SAMEA2580186" }
    , { gmbcID = "GMBC10.007_229", quality = HighQ, origin = "SAMN05545074" }
    , { gmbcID = "GMBC10.007_230", quality = HighQ, origin = "SAMN05545073" }
    , { gmbcID = "GMBC10.007_231", quality = HighQ, origin = "SAMN05545072" }
    , { gmbcID = "GMBC10.007_232", quality = HighQ, origin = "SAMN05545044" }
    , { gmbcID = "GMBC10.007_233", quality = HighQ, origin = "SAMN05545015" }
    , { gmbcID = "GMBC10.007_262", quality = HighQ, origin = "SAMEA4378297" }
    , { gmbcID = "GMBC10.007_331", quality = HighQ, origin = "SAMN05545075" }
    , { gmbcID = "GMBC10.007_332", quality = HighQ, origin = "SAMN04436933" }
    , { gmbcID = "GMBC10.007_366", quality = HighQ, origin = "SAMN01780039" }
    , { gmbcID = "GMBC10.007_381", quality = HighQ, origin = "SAMEA2580168" }
    , { gmbcID = "GMBC10.007_393", quality = HighQ, origin = "SAMN05826868" }
    , { gmbcID = "GMBC10.007_471", quality = HighQ, origin = "SAMEA2580028" }
    , { gmbcID = "GMBC10.007_507", quality = HighQ, origin = "SAMN04436760" }
    , { gmbcID = "GMBC10.007_525", quality = HighQ, origin = "SAMEA3541485" }
    , { gmbcID = "GMBC10.007_555", quality = HighQ, origin = "SAMEA3449385" }
    , { gmbcID = "GMBC10.007_564", quality = HighQ, origin = "SAMEA3708655" }
    , { gmbcID = "GMBC10.007_634", quality = HighQ, origin = "SAMEA2737676" }
    , { gmbcID = "GMBC10.007_687", quality = HighQ, origin = "SAMN02334021" }
    , { gmbcID = "GMBC10.007_706", quality = HighQ, origin = "SAMEA2737675" }
    , { gmbcID = "GMBC10.007_727", quality = HighQ, origin = "SRX1670024" }
    , { gmbcID = "GMBC10.007_787", quality = HighQ, origin = "SAMEA3708566" }
    , { gmbcID = "GMBC10.007_962", quality = HighQ, origin = "SAMEA2580095" }
    , { gmbcID = "GMBC10.008_112", quality = HighQ, origin = "SAMEA4378322" }
    , { gmbcID = "GMBC10.008_167", quality = HighQ, origin = "SAMN05826525" }
    , { gmbcID = "GMBC10.008_175", quality = HighQ, origin = "SAMEA2580159" }
    , { gmbcID = "GMBC10.008_193", quality = HighQ, origin = "SAMEA2580166" }
    , { gmbcID = "GMBC10.008_244", quality = HighQ, origin = "SAMN05544999" }
    , { gmbcID = "GMBC10.008_245", quality = HighQ, origin = "SAMN05544989" }
    , { gmbcID = "GMBC10.008_346", quality = HighQ, origin = "SAMEA3541565" }
    , { gmbcID = "GMBC10.008_493", quality = HighQ, origin = "SAMN00691193" }
    , { gmbcID = "GMBC10.008_525", quality = HighQ, origin = "SAMEA2580198" }
    , { gmbcID = "GMBC10.008_589", quality = HighQ, origin = "SAMN01795862" }
    , { gmbcID = "GMBC10.008_618", quality = HighQ, origin = "SAMEA3708814" }
    , { gmbcID = "GMBC10.008_621", quality = HighQ, origin = "SAMEA2580259" }
    , { gmbcID = "GMBC10.008_645", quality = HighQ, origin = "SAMEA3136734" }
    , { gmbcID = "GMBC10.008_731", quality = HighQ, origin = "SAMEA2582058" }
    , { gmbcID = "GMBC10.008_732", quality = HighQ, origin = "SAMEA2580156" }
    , { gmbcID = "GMBC10.008_890", quality = HighQ, origin = "SAMEA2580067" }
    , { gmbcID = "GMBC10.008_916", quality = HighQ, origin = "SAMEA3951721" }
    , { gmbcID = "GMBC10.008_926", quality = HighQ, origin = "SAMEA3708813" }
    , { gmbcID = "GMBC10.008_978", quality = HighQ, origin = "SAMEA3136766" }
    , { gmbcID = "GMBC10.009_061", quality = HighQ, origin = "SAMEA2737792" }
    , { gmbcID = "GMBC10.009_170", quality = HighQ, origin = "SAMN03025395" }
    , { gmbcID = "GMBC10.009_219", quality = HighQ, origin = "SAMN04436799" }
    , { gmbcID = "GMBC10.009_347", quality = HighQ, origin = "SAMEA2579909" }
    , { gmbcID = "GMBC10.009_354", quality = HighQ, origin = "SAMEA2580119" }
    , { gmbcID = "GMBC10.009_435", quality = HighQ, origin = "SAMEA3136723" }
    , { gmbcID = "GMBC10.009_439", quality = HighQ, origin = "SAMEA2580286" }
    , { gmbcID = "GMBC10.009_459", quality = HighQ, origin = "SAMEA3708825" }
    , { gmbcID = "GMBC10.009_467", quality = HighQ, origin = "SAMN00990354" }
    , { gmbcID = "GMBC10.009_508", quality = HighQ, origin = "SAMEA3136769" }
    , { gmbcID = "GMBC10.009_877", quality = HighQ, origin = "SAMN05545034" }
    , { gmbcID = "GMBC10.009_920", quality = HighQ, origin = "SAMN04436896" }
    , { gmbcID = "GMBC10.010_033", quality = HighQ, origin = "SAMEA2466991" }
    , { gmbcID = "GMBC10.010_102", quality = HighQ, origin = "SAMEA3449254" }
    , { gmbcID = "GMBC10.010_144", quality = HighQ, origin = "SAMEA3449417" }
    , { gmbcID = "GMBC10.010_189", quality = HighQ, origin = "SAMEA3708729" }
    , { gmbcID = "GMBC10.010_220", quality = HighQ, origin = "SAMEA2580015" }
    , { gmbcID = "GMBC10.010_261", quality = HighQ, origin = "SAMEA3879578" }
    , { gmbcID = "GMBC10.010_343", quality = HighQ, origin = "SAMEA3708516" }
    , { gmbcID = "GMBC10.010_372", quality = HighQ, origin = "SAMEA2148642" }
    , { gmbcID = "GMBC10.010_397", quality = HighQ, origin = "SAMEA2581889" }
    , { gmbcID = "GMBC10.010_473", quality = HighQ, origin = "SAMEA3664783" }
    , { gmbcID = "GMBC10.010_584", quality = HighQ, origin = "SAMEA4378285" }
    , { gmbcID = "GMBC10.010_601", quality = HighQ, origin = "SAMN05827182" }
    , { gmbcID = "GMBC10.010_617", quality = HighQ, origin = "SAMN04436967" }
    , { gmbcID = "GMBC10.010_641", quality = HighQ, origin = "SAMEA2467019" }
    , { gmbcID = "GMBC10.010_644", quality = HighQ, origin = "SAMEA2580109" }
    , { gmbcID = "GMBC10.010_850", quality = HighQ, origin = "SAMN04436761" }
    , { gmbcID = "GMBC10.010_862", quality = HighQ, origin = "SAMEA2580183" }
    , { gmbcID = "GMBC10.010_867", quality = HighQ, origin = "SAMN04436726" }
    , { gmbcID = "GMBC10.010_929", quality = HighQ, origin = "SAMN03398682" }
    , { gmbcID = "GMBC10.010_972", quality = HighQ, origin = "SAMEA2580011" }
    , { gmbcID = "GMBC10.010_980", quality = HighQ, origin = "SAMEA3664926" }
    , { gmbcID = "GMBC10.011_047", quality = HighQ, origin = "SAMN00691554" }
    , { gmbcID = "GMBC10.011_120", quality = HighQ, origin = "SAMEA4378240" }
    , { gmbcID = "GMBC10.011_192", quality = HighQ, origin = "SAMEA2582037" }
    , { gmbcID = "GMBC10.011_258", quality = HighQ, origin = "SAMEA3541548" }
    , { gmbcID = "GMBC10.011_285", quality = HighQ, origin = "SAMEA2737810" }
    , { gmbcID = "GMBC10.011_318", quality = HighQ, origin = "SAMEA2579916" }
    , { gmbcID = "GMBC10.011_331", quality = HighQ, origin = "SAMEA2737760" }
    , { gmbcID = "GMBC10.011_345", quality = HighQ, origin = "SAMEA2737777" }
    , { gmbcID = "GMBC10.011_393", quality = HighQ, origin = "SAMN00990327" }
    , { gmbcID = "GMBC10.011_403", quality = HighQ, origin = "SAMN05545033" }
    , { gmbcID = "GMBC10.011_431", quality = HighQ, origin = "SAMN02675515" }
    , { gmbcID = "GMBC10.011_443", quality = HighQ, origin = "SAMN05545004" }
    , { gmbcID = "GMBC10.011_651", quality = HighQ, origin = "SAMN01774302" }
    , { gmbcID = "GMBC10.011_740", quality = HighQ, origin = "SAMEA2580116" }
    , { gmbcID = "GMBC10.011_809", quality = HighQ, origin = "SAMEA3136764" }
    , { gmbcID = "GMBC10.012_072", quality = HighQ, origin = "SAMEA2579996" }
    , { gmbcID = "GMBC10.012_109", quality = HighQ, origin = "SAMN04436731" }
    , { gmbcID = "GMBC10.012_111", quality = HighQ, origin = "SAMEA3708687" }
    , { gmbcID = "GMBC10.012_116", quality = HighQ, origin = "SAMEA2580206" }
    , { gmbcID = "GMBC10.012_165", quality = HighQ, origin = "SAMEA4378238" }
    , { gmbcID = "GMBC10.012_290", quality = HighQ, origin = "SAMEA2338795" }
    , { gmbcID = "GMBC10.012_382", quality = HighQ, origin = "SAMEA3449309" }
    , { gmbcID = "GMBC10.012_416", quality = HighQ, origin = "SAMN01795801" }
    , { gmbcID = "GMBC10.012_442", quality = HighQ, origin = "SAMEA3449245" }
    , { gmbcID = "GMBC10.012_495", quality = HighQ, origin = "SAMEA3708684" }
    , { gmbcID = "GMBC10.012_498", quality = HighQ, origin = "SAMEA2580275" }
    , { gmbcID = "GMBC10.012_611", quality = HighQ, origin = "SAMEA2466993" }
    , { gmbcID = "GMBC10.012_643", quality = HighQ, origin = "SAMEA4378283" }
    , { gmbcID = "GMBC10.012_665", quality = HighQ, origin = "SAMEA2579991" }
    , { gmbcID = "GMBC10.012_735", quality = HighQ, origin = "SAMN04436779" }
    , { gmbcID = "GMBC10.012_837", quality = HighQ, origin = "SAMEA2737679" }
    , { gmbcID = "GMBC10.012_838", quality = HighQ, origin = "SAMEA2580091" }
    , { gmbcID = "GMBC10.012_858", quality = HighQ, origin = "SAMEA2580245" }
    , { gmbcID = "GMBC10.012_906", quality = HighQ, origin = "SAMEA3708595" }
    , { gmbcID = "GMBC10.012_919", quality = HighQ, origin = "SAMEA2338843" }
    , { gmbcID = "GMBC10.012_977", quality = HighQ, origin = "SAMEA3136679" }
    , { gmbcID = "GMBC10.013_027", quality = HighQ, origin = "SAMEA2579976" }
    , { gmbcID = "GMBC10.013_064", quality = HighQ, origin = "SAMN05826893" }
    , { gmbcID = "GMBC10.013_099", quality = HighQ, origin = "SAMEA2737821" }
    , { gmbcID = "GMBC10.013_100", quality = HighQ, origin = "SAMEA2737772" }
    , { gmbcID = "GMBC10.013_101", quality = HighQ, origin = "SAMEA2579928" }
    , { gmbcID = "GMBC10.013_132", quality = HighQ, origin = "SAMN00998844" }
    , { gmbcID = "GMBC10.013_140", quality = HighQ, origin = "SAMN01795795" }
    , { gmbcID = "GMBC10.013_180", quality = HighQ, origin = "SAMN05545027" }
    , { gmbcID = "GMBC10.013_181", quality = HighQ, origin = "SAMN05545005" }
    , { gmbcID = "GMBC10.013_182", quality = HighQ, origin = "SAMN05545003" }
    , { gmbcID = "GMBC10.013_445", quality = HighQ, origin = "SAMN04436848" }
    , { gmbcID = "GMBC10.013_471", quality = HighQ, origin = "SAMEA2580262" }
    , { gmbcID = "GMBC10.013_472", quality = HighQ, origin = "SAMEA2580051" }
    , { gmbcID = "GMBC10.013_790", quality = HighQ, origin = "SAMN04436816" }
    , { gmbcID = "GMBC10.013_865", quality = HighQ, origin = "SAMEA2737730" }
    , { gmbcID = "GMBC10.013_907", quality = HighQ, origin = "SAMEA3708855" }
    , { gmbcID = "GMBC10.014_080", quality = HighQ, origin = "SAMEA2467012" }
    , { gmbcID = "GMBC10.014_129", quality = HighQ, origin = "SAMEA2580014" }
    , { gmbcID = "GMBC10.014_194", quality = HighQ, origin = "SAMN04436801" }
    , { gmbcID = "GMBC10.014_210", quality = HighQ, origin = "SAMEA2580291" }
    , { gmbcID = "GMBC10.014_292", quality = HighQ, origin = "SAMEA2467032" }
    , { gmbcID = "GMBC10.014_300", quality = HighQ, origin = "SAMEA2580022" }
    , { gmbcID = "GMBC10.014_321", quality = HighQ, origin = "SAMN05827158" }
    , { gmbcID = "GMBC10.014_434", quality = HighQ, origin = "SAMEA3708685" }
    , { gmbcID = "GMBC10.014_481", quality = HighQ, origin = "SAMEA2580086" }
    , { gmbcID = "GMBC10.014_510", quality = HighQ, origin = "SAMEA3449271" }
    , { gmbcID = "GMBC10.014_535", quality = HighQ, origin = "SAMN02334124" }
    , { gmbcID = "GMBC10.014_597", quality = HighQ, origin = "SAMEA3136724" }
    , { gmbcID = "GMBC10.014_676", quality = HighQ, origin = "SAMEA2737671" }
    , { gmbcID = "GMBC10.014_775", quality = HighQ, origin = "SAMEA2338635" }
    , { gmbcID = "GMBC10.014_818", quality = HighQ, origin = "SAMEA3664803" }
    , { gmbcID = "GMBC10.014_975", quality = HighQ, origin = "SAMN04436656" }
    , { gmbcID = "GMBC10.015_088", quality = HighQ, origin = "SAMEA3664946" }
    , { gmbcID = "GMBC10.015_112", quality = HighQ, origin = "SAMEA2580236" }
    , { gmbcID = "GMBC10.015_179", quality = HighQ, origin = "SAMEA2466946" }
    , { gmbcID = "GMBC10.015_213", quality = HighQ, origin = "SRS017521" }
    , { gmbcID = "GMBC10.015_225", quality = HighQ, origin = "SAMEA2737864" }
    , { gmbcID = "GMBC10.015_247", quality = HighQ, origin = "SAMN04436925" }
    , { gmbcID = "GMBC10.015_299", quality = HighQ, origin = "SAMN01773464" }
    , { gmbcID = "GMBC10.015_463", quality = HighQ, origin = "SAMN05826517" }
    , { gmbcID = "GMBC10.015_499", quality = HighQ, origin = "SAMEA2579990" }
    , { gmbcID = "GMBC10.015_513", quality = HighQ, origin = "SAMN02675513" }
    , { gmbcID = "GMBC10.015_636", quality = HighQ, origin = "SAMEA2580191" }
    , { gmbcID = "GMBC10.015_879", quality = HighQ, origin = "SAMEA3879548" }
    , { gmbcID = "GMBC10.015_998", quality = HighQ, origin = "SAMN05827194" }
    , { gmbcID = "GMBC10.016_053", quality = HighQ, origin = "SAMEA1906576" }
    , { gmbcID = "GMBC10.016_064", quality = HighQ, origin = "SAMEA1906526" }
    , { gmbcID = "GMBC10.016_152", quality = HighQ, origin = "SAMEA2338796" }
    , { gmbcID = "GMBC10.016_211", quality = HighQ, origin = "SAMEA2580178" }
    , { gmbcID = "GMBC10.016_304", quality = HighQ, origin = "SAMN04436649" }
    , { gmbcID = "GMBC10.016_374", quality = HighQ, origin = "SAMN04436817" }
    , { gmbcID = "GMBC10.016_488", quality = HighQ, origin = "SAMEA3541562" }
    , { gmbcID = "GMBC10.016_560", quality = HighQ, origin = "SAMEA2580256" }
    , { gmbcID = "GMBC10.016_562", quality = HighQ, origin = "SAMEA2579934" }
    , { gmbcID = "GMBC10.016_570", quality = HighQ, origin = "SAMEA3664986" }
    , { gmbcID = "GMBC10.016_585", quality = HighQ, origin = "SAMN00696741" }
    , { gmbcID = "GMBC10.016_631", quality = HighQ, origin = "SAMN00699770" }
    , { gmbcID = "GMBC10.016_732", quality = HighQ, origin = "SAMN03399873" }
    , { gmbcID = "GMBC10.016_764", quality = HighQ, origin = "SAMN02334186" }
    , { gmbcID = "GMBC10.017_010", quality = HighQ, origin = "SAMN05545017" }
    , { gmbcID = "GMBC10.017_013", quality = HighQ, origin = "SAMEA2579926" }
    , { gmbcID = "GMBC10.017_262", quality = HighQ, origin = "SAMEA3951757" }
    , { gmbcID = "GMBC10.017_436", quality = HighQ, origin = "SAMEA3708739" }
    , { gmbcID = "GMBC10.017_544", quality = HighQ, origin = "SAMN05544990" }
    , { gmbcID = "GMBC10.017_556", quality = HighQ, origin = "SAMEA2580158" }
    , { gmbcID = "GMBC10.017_563", quality = HighQ, origin = "SAMN02334096" }
    , { gmbcID = "GMBC10.017_589", quality = HighQ, origin = "SAMN01780289" }
    , { gmbcID = "GMBC10.017_719", quality = HighQ, origin = "SAMN04436965" }
    , { gmbcID = "GMBC10.017_988", quality = HighQ, origin = "SAMN05544992" }
    , { gmbcID = "GMBC10.018_005", quality = HighQ, origin = "SAMEA4378310" }
    , { gmbcID = "GMBC10.018_024", quality = HighQ, origin = "SAMEA3663240" }
    , { gmbcID = "GMBC10.018_082", quality = HighQ, origin = "SAMEA2581899" }
    , { gmbcID = "GMBC10.018_111", quality = HighQ, origin = "SAMEA2579949" }
    , { gmbcID = "GMBC10.018_212", quality = HighQ, origin = "SAMEA2579948" }
    , { gmbcID = "GMBC10.018_220", quality = HighQ, origin = "SAMN04261186" }
    , { gmbcID = "GMBC10.018_226", quality = HighQ, origin = "SAMEA3879534" }
    , { gmbcID = "GMBC10.018_292", quality = HighQ, origin = "SAMEA2579970" }
    , { gmbcID = "GMBC10.018_393", quality = HighQ, origin = "SAMN04436858" }
    , { gmbcID = "GMBC10.018_465", quality = HighQ, origin = "SAMN04436964" }
    , { gmbcID = "GMBC10.018_469", quality = HighQ, origin = "SAMEA2581927" }
    , { gmbcID = "GMBC10.018_513", quality = HighQ, origin = "SAMN05826920" }
    , { gmbcID = "GMBC10.018_587", quality = HighQ, origin = "SAMEA2580070" }
    , { gmbcID = "GMBC10.018_630", quality = HighQ, origin = "SAMN05827117" }
    , { gmbcID = "GMBC10.018_638", quality = HighQ, origin = "SAMEA3951639" }
    , { gmbcID = "GMBC10.018_642", quality = HighQ, origin = "SAMEA3708623" }
    , { gmbcID = "GMBC10.019_044", quality = HighQ, origin = "SAMEA3663206" }
    , { gmbcID = "GMBC10.019_237", quality = HighQ, origin = "SAMEA2579944" }
    , { gmbcID = "GMBC10.019_374", quality = HighQ, origin = "SAMEA3708517" }
    , { gmbcID = "GMBC10.019_420", quality = HighQ, origin = "SAMEA3708768" }
    , { gmbcID = "GMBC10.019_427", quality = HighQ, origin = "SAMEA3664840" }
    , { gmbcID = "GMBC10.019_579", quality = HighQ, origin = "SAMEA2737842" }
    , { gmbcID = "GMBC10.019_738", quality = HighQ, origin = "SAMN05545020" }
    , { gmbcID = "GMBC10.019_739", quality = HighQ, origin = "SAMN05544800" }
    , { gmbcID = "GMBC10.019_746", quality = HighQ, origin = "SAMEA3449341" }
    , { gmbcID = "GMBC10.019_801", quality = HighQ, origin = "SAMN04436819" }
    , { gmbcID = "GMBC10.019_805", quality = HighQ, origin = "SAMEA2042108" }
    , { gmbcID = "GMBC10.020_055", quality = HighQ, origin = "SAMEA2579927" }
    , { gmbcID = "GMBC10.020_123", quality = HighQ, origin = "SAMEA3879515" }
    , { gmbcID = "GMBC10.020_234", quality = HighQ, origin = "SAMEA3664900" }
    , { gmbcID = "GMBC10.020_338", quality = HighQ, origin = "SAMN02676173" }
    , { gmbcID = "GMBC10.020_386", quality = HighQ, origin = "SAMN01795863" }
    , { gmbcID = "GMBC10.020_400", quality = HighQ, origin = "SAMEA2579915" }
    , { gmbcID = "GMBC10.020_437", quality = HighQ, origin = "SAMEA2737865" }
    , { gmbcID = "GMBC10.020_461", quality = HighQ, origin = "SAMEA2580138" }
    , { gmbcID = "GMBC10.020_899", quality = HighQ, origin = "SAMEA3708865" }
    , { gmbcID = "GMBC10.021_024", quality = HighQ, origin = "SAMN04261239" }
    , { gmbcID = "GMBC10.021_054", quality = HighQ, origin = "SAMN04436963" }
    , { gmbcID = "GMBC10.021_062", quality = HighQ, origin = "SAMEA3879559" }
    , { gmbcID = "GMBC10.021_098", quality = HighQ, origin = "SAMEA3449206" }
    , { gmbcID = "GMBC10.021_100", quality = HighQ, origin = "SAMEA2737815" }
    , { gmbcID = "GMBC10.021_229", quality = HighQ, origin = "SAMN01780292" }
    , { gmbcID = "GMBC10.021_247", quality = HighQ, origin = "SAMEA3708478" }
    , { gmbcID = "GMBC10.021_313", quality = HighQ, origin = "SAMN05545084" }
    , { gmbcID = "GMBC10.021_368", quality = HighQ, origin = "SAMN02334005" }
    , { gmbcID = "GMBC10.021_377", quality = HighQ, origin = "SAMN03398834" }
    , { gmbcID = "GMBC10.021_451", quality = HighQ, origin = "SAMEA3708829" }
    , { gmbcID = "GMBC10.021_486", quality = HighQ, origin = "SAMEA3879537" }
    , { gmbcID = "GMBC10.021_563", quality = HighQ, origin = "SAMN05545046" }
    , { gmbcID = "GMBC10.021_564", quality = HighQ, origin = "SAMN05545029" }
    , { gmbcID = "GMBC10.021_574", quality = HighQ, origin = "SAMEA2338794" }
    , { gmbcID = "GMBC10.021_681", quality = HighQ, origin = "SAMEA3708489" }
    , { gmbcID = "GMBC10.021_833", quality = HighQ, origin = "SAMEA2737848" }
    , { gmbcID = "GMBC10.022_077", quality = HighQ, origin = "SAMEA3708513" }
    , { gmbcID = "GMBC10.022_249", quality = HighQ, origin = "SAMN02675512" }
    , { gmbcID = "GMBC10.022_588", quality = HighQ, origin = "SAMN02333970" }
    , { gmbcID = "GMBC10.022_735", quality = HighQ, origin = "SAMEA3663283" }
    , { gmbcID = "GMBC10.022_867", quality = HighQ, origin = "SAMN04261229" }
    , { gmbcID = "GMBC10.023_288", quality = HighQ, origin = "SAMEA2580298" }
    , { gmbcID = "GMBC10.023_320", quality = HighQ, origin = "SAMN01780295" }
    , { gmbcID = "GMBC10.023_347", quality = HighQ, origin = "SAMN05545088" }
    , { gmbcID = "GMBC10.023_507", quality = HighQ, origin = "SAMN01779979" }
    , { gmbcID = "GMBC10.023_575", quality = HighQ, origin = "SAMEA2737741" }
    , { gmbcID = "GMBC10.023_828", quality = HighQ, origin = "SAMN05545042" }
    , { gmbcID = "GMBC10.024_105", quality = HighQ, origin = "SAMN01795885" }
    , { gmbcID = "GMBC10.024_118", quality = HighQ, origin = "SAMEA2582111" }
    , { gmbcID = "GMBC10.024_164", quality = HighQ, origin = "SAMEA3664851" }
    , { gmbcID = "GMBC10.024_225", quality = HighQ, origin = "SAMEA2737869" }
    , { gmbcID = "GMBC10.024_268", quality = HighQ, origin = "SAMEA2580010" }
    , { gmbcID = "GMBC10.024_405", quality = HighQ, origin = "SAMN03398792" }
    , { gmbcID = "GMBC10.024_609", quality = HighQ, origin = "SAMN01795867" }
    , { gmbcID = "GMBC10.024_655", quality = HighQ, origin = "SAMEA4378317" }
    , { gmbcID = "GMBC10.024_707", quality = HighQ, origin = "SAMEA2737840" }
    , { gmbcID = "GMBC10.024_783", quality = HighQ, origin = "SAMN01780050" }
    , { gmbcID = "GMBC10.024_831", quality = HighQ, origin = "SAMN02676163" }
    , { gmbcID = "GMBC10.024_841", quality = HighQ, origin = "SAMN04261338" }
    , { gmbcID = "GMBC10.024_885", quality = HighQ, origin = "SAMEA2581926" }
    , { gmbcID = "GMBC10.025_052", quality = HighQ, origin = "SAMN03399937" }
    , { gmbcID = "GMBC10.025_131", quality = HighQ, origin = "SAMEA2737649" }
    , { gmbcID = "GMBC10.025_310", quality = HighQ, origin = "SAMEA2580161" }
    , { gmbcID = "GMBC10.025_339", quality = HighQ, origin = "SAMEA4378284" }
    , { gmbcID = "GMBC10.025_367", quality = HighQ, origin = "SAMEA3541541" }
    , { gmbcID = "GMBC10.025_462", quality = HighQ, origin = "SAMEA2737875" }
    , { gmbcID = "GMBC10.025_464", quality = HighQ, origin = "SAMEA2579975" }
    , { gmbcID = "GMBC10.025_613", quality = HighQ, origin = "SAMN02334139" }
    , { gmbcID = "GMBC10.025_706", quality = HighQ, origin = "SAMN05827147" }
    , { gmbcID = "GMBC10.025_733", quality = HighQ, origin = "SAMEA2580056" }
    , { gmbcID = "GMBC10.026_048", quality = HighQ, origin = "SAMEA1906497" }
    , { gmbcID = "GMBC10.026_172", quality = HighQ, origin = "SAMEA2338820" }
    , { gmbcID = "GMBC10.026_190", quality = HighQ, origin = "SAMEA3708730" }
    , { gmbcID = "GMBC10.026_483", quality = HighQ, origin = "SAMN02333919" }
    , { gmbcID = "GMBC10.026_686", quality = HighQ, origin = "SAMN05827242" }
    , { gmbcID = "GMBC10.026_769", quality = HighQ, origin = "SAMN00696742" }
    , { gmbcID = "GMBC10.026_797", quality = HighQ, origin = "SAMEA2580075" }
    , { gmbcID = "GMBC10.026_838", quality = HighQ, origin = "SAMEA2579956" }
    , { gmbcID = "GMBC10.026_918", quality = HighQ, origin = "SAMEA2737690" }
    , { gmbcID = "GMBC10.027_189", quality = HighQ, origin = "SAMEA2580122" }
    , { gmbcID = "GMBC10.027_239", quality = HighQ, origin = "SAMN05545008" }
    , { gmbcID = "GMBC10.027_322", quality = HighQ, origin = "SAMN02333960" }
    , { gmbcID = "GMBC10.027_430", quality = HighQ, origin = "SAMEA2582160" }
    , { gmbcID = "GMBC10.027_646", quality = HighQ, origin = "SAMEA2580196" }
    , { gmbcID = "GMBC10.027_744", quality = HighQ, origin = "SAMEA2042395" }
    , { gmbcID = "GMBC10.027_781", quality = HighQ, origin = "SAMEA2580274" }
    , { gmbcID = "GMBC10.027_801", quality = HighQ, origin = "SAMEA3951690" }
    , { gmbcID = "GMBC10.027_847", quality = HighQ, origin = "SAMEA2737680" }
    , { gmbcID = "GMBC10.027_872", quality = HighQ, origin = "SAMEA3708702" }
    , { gmbcID = "GMBC10.027_943", quality = HighQ, origin = "SAMN05827156" }
    , { gmbcID = "GMBC10.028_051", quality = HighQ, origin = "SAMEA3541477" }
    , { gmbcID = "GMBC10.028_184", quality = HighQ, origin = "SAMN05545076" }
    , { gmbcID = "GMBC10.028_209", quality = HighQ, origin = "SAMEA3449316" }
    , { gmbcID = "GMBC10.028_347", quality = HighQ, origin = "SAMEA2580250" }
    , { gmbcID = "GMBC10.028_555", quality = HighQ, origin = "SAMN04436724" }
    , { gmbcID = "GMBC10.028_594", quality = HighQ, origin = "SAMEA2737746" }
    , { gmbcID = "GMBC10.028_622", quality = HighQ, origin = "SAMEA2467067" }
    , { gmbcID = "GMBC10.028_729", quality = HighQ, origin = "SAMEA3136634" }
    , { gmbcID = "GMBC10.029_029", quality = HighQ, origin = "SAMEA3541530" }
    , { gmbcID = "GMBC10.029_049", quality = HighQ, origin = "SAMN04436602" }
    , { gmbcID = "GMBC10.029_092", quality = HighQ, origin = "SAMEA2580165" }
    , { gmbcID = "GMBC10.029_255", quality = HighQ, origin = "SAMEA1970897" }
    , { gmbcID = "GMBC10.029_446", quality = HighQ, origin = "SAMEA2466979" }
    , { gmbcID = "GMBC10.029_447", quality = HighQ, origin = "SAMEA2338748" }
    , { gmbcID = "GMBC10.029_502", quality = HighQ, origin = "SAMN05545086" }
    , { gmbcID = "GMBC10.029_513", quality = HighQ, origin = "SAMN04436727" }
    , { gmbcID = "GMBC10.029_519", quality = HighQ, origin = "SAMEA3879604" }
    , { gmbcID = "GMBC10.029_572", quality = HighQ, origin = "SAMEA3449344" }
    , { gmbcID = "GMBC10.029_582", quality = HighQ, origin = "SAMN04436832" }
    , { gmbcID = "GMBC10.029_704", quality = HighQ, origin = "SAMEA2580042" }
    , { gmbcID = "GMBC10.030_138", quality = HighQ, origin = "SAMEA2466960" }
    , { gmbcID = "GMBC10.030_221", quality = HighQ, origin = "SAMEA1906448" }
    , { gmbcID = "GMBC10.030_560", quality = HighQ, origin = "SAMEA3708744" }
    , { gmbcID = "GMBC10.030_561", quality = HighQ, origin = "SAMEA3708576" }
    , { gmbcID = "GMBC10.030_580", quality = HighQ, origin = "SAMEA4378261" }
    , { gmbcID = "GMBC10.030_740", quality = HighQ, origin = "SAMEA2579921" }
    , { gmbcID = "GMBC10.030_782", quality = HighQ, origin = "SAMN02333958" }
    , { gmbcID = "GMBC10.030_805", quality = HighQ, origin = "SAMEA2580134" }
    , { gmbcID = "GMBC10.031_142", quality = HighQ, origin = "SAMEA3449249" }
    , { gmbcID = "GMBC10.031_417", quality = HighQ, origin = "SAMEA3449315" }
    , { gmbcID = "GMBC10.031_452", quality = HighQ, origin = "SAMEA3664774" }
    , { gmbcID = "GMBC10.031_500", quality = HighQ, origin = "SAMEA2580270" }
    , { gmbcID = "GMBC10.031_782", quality = HighQ, origin = "SAMEA3387516" }
    , { gmbcID = "GMBC10.031_864", quality = HighQ, origin = "SAMEA3708521" }
    , { gmbcID = "GMBC10.032_032", quality = HighQ, origin = "SAMN05545039" }
    , { gmbcID = "GMBC10.032_118", quality = HighQ, origin = "SAMN02675510" }
    , { gmbcID = "GMBC10.032_120", quality = MedQ, origin = "SAMEA3951630" }
    , { gmbcID = "GMBC10.032_127", quality = HighQ, origin = "SAMEA3664870" }
    , { gmbcID = "GMBC10.032_297", quality = HighQ, origin = "SAMN04436717" }
    , { gmbcID = "GMBC10.032_606", quality = HighQ, origin = "SAMEA2737754" }
    , { gmbcID = "GMBC10.032_869", quality = HighQ, origin = "SAMN03271081" }
    , { gmbcID = "GMBC10.033_045", quality = HighQ, origin = "SAMEA3136736" }
    , { gmbcID = "GMBC10.033_204", quality = HighQ, origin = "SAMEA3541525" }
    , { gmbcID = "GMBC10.033_277", quality = HighQ, origin = "SAMEA3951641" }
    , { gmbcID = "GMBC10.033_474", quality = HighQ, origin = "SAMEA3708859" }
    , { gmbcID = "GMBC10.033_481", quality = HighQ, origin = "SAMEA3541596" }
    , { gmbcID = "GMBC10.033_513", quality = HighQ, origin = "SAMEA2737849" }
    , { gmbcID = "GMBC10.033_772", quality = HighQ, origin = "SRX1670018" }
    , { gmbcID = "GMBC10.033_827", quality = HighQ, origin = "SAMEA3708666" }
    , { gmbcID = "GMBC10.033_836", quality = HighQ, origin = "SAMEA2580241" }
    , { gmbcID = "GMBC10.033_919", quality = HighQ, origin = "SAMN02676197" }
    , { gmbcID = "GMBC10.034_123", quality = HighQ, origin = "SAMN03271145" }
    , { gmbcID = "GMBC10.034_161", quality = HighQ, origin = "SAMEA2737737" }
    , { gmbcID = "GMBC10.034_271", quality = HighQ, origin = "SAMEA2580057" }
    , { gmbcID = "GMBC10.034_276", quality = HighQ, origin = "SAMEA2042365" }
    , { gmbcID = "GMBC10.034_278", quality = HighQ, origin = "SAMEA4378281" }
    , { gmbcID = "GMBC10.034_282", quality = HighQ, origin = "SAMN04436908" }
    , { gmbcID = "GMBC10.034_477", quality = HighQ, origin = "SAMEA2338657" }
    , { gmbcID = "GMBC10.034_773", quality = HighQ, origin = "SAMEA3541481" }
    , { gmbcID = "GMBC10.035_115", quality = HighQ, origin = "SAMEA2737701" }
    , { gmbcID = "GMBC10.035_351", quality = HighQ, origin = "SAMEA3136692" }
    , { gmbcID = "GMBC10.035_381", quality = HighQ, origin = "SAMEA2579998" }
    , { gmbcID = "GMBC10.035_460", quality = HighQ, origin = "SAMEA2580024" }
    , { gmbcID = "GMBC10.035_555", quality = HighQ, origin = "SAMEA2737702" }
    , { gmbcID = "GMBC10.035_666", quality = HighQ, origin = "SAMEA2580289" }
    , { gmbcID = "GMBC10.035_859", quality = HighQ, origin = "SAMEA3708690" }
    , { gmbcID = "GMBC10.035_950", quality = HighQ, origin = "SAMN02334003" }
    , { gmbcID = "GMBC10.035_953", quality = HighQ, origin = "SAMEA3541469" }
    , { gmbcID = "GMBC10.036_135", quality = HighQ, origin = "SAMEA3541520" }
    , { gmbcID = "GMBC10.036_155", quality = MedQ, origin = "SAMEA3708571" }
    , { gmbcID = "GMBC10.036_271", quality = HighQ, origin = "SAMEA2737684" }
    , { gmbcID = "GMBC10.036_281", quality = MedQ, origin = "SAMEA3449255" }
    , { gmbcID = "GMBC10.036_284", quality = HighQ, origin = "SAMEA2580131" }
    , { gmbcID = "GMBC10.036_354", quality = HighQ, origin = "SAMEA3708484" }
    , { gmbcID = "GMBC10.036_416", quality = HighQ, origin = "SAMEA4378308" }
    , { gmbcID = "GMBC10.036_431", quality = HighQ, origin = "SAMEA3708732" }
    , { gmbcID = "GMBC10.036_525", quality = HighQ, origin = "SRS146888" }
    , { gmbcID = "GMBC10.036_563", quality = HighQ, origin = "SAMN03270111" }
    , { gmbcID = "GMBC10.036_694", quality = HighQ, origin = "SAMEA3708801" }
    , { gmbcID = "GMBC10.036_801", quality = HighQ, origin = "SAMEA3665069" }
    , { gmbcID = "GMBC10.037_071", quality = HighQ, origin = "SAMEA2582029" }
    , { gmbcID = "GMBC10.037_217", quality = HighQ, origin = "SAMEA3387447" }
    , { gmbcID = "GMBC10.037_224", quality = HighQ, origin = "SAMEA2338790" }
    , { gmbcID = "GMBC10.037_304", quality = HighQ, origin = "SAMEA3541523" }
    , { gmbcID = "GMBC10.037_661", quality = MedQ, origin = "SAMEA3879580" }
    , { gmbcID = "GMBC10.037_714", quality = HighQ, origin = "SAMN02334224" }
    , { gmbcID = "GMBC10.037_831", quality = HighQ, origin = "SAMN05827175" }
    , { gmbcID = "GMBC10.037_891", quality = HighQ, origin = "SAMN04261241" }
    , { gmbcID = "GMBC10.037_898", quality = HighQ, origin = "SAMEA3664673" }
    , { gmbcID = "GMBC10.038_020", quality = HighQ, origin = "SAMEA3708573" }
    , { gmbcID = "GMBC10.038_046", quality = HighQ, origin = "SAMN05826586" }
    , { gmbcID = "GMBC10.038_047", quality = HighQ, origin = "SAMN04436849" }
    , { gmbcID = "GMBC10.038_107", quality = HighQ, origin = "SAMN02334031" }
    , { gmbcID = "GMBC10.038_198", quality = HighQ, origin = "SAMEA3541532" }
    , { gmbcID = "GMBC10.038_554", quality = MedQ, origin = "SAMN04436924" }
    , { gmbcID = "GMBC10.038_687", quality = MedQ, origin = "SAMN05545031" }
    , { gmbcID = "GMBC10.038_813", quality = HighQ, origin = "SAMEA2580120" }
    , { gmbcID = "GMBC10.038_820", quality = HighQ, origin = "SAMN02334225" }
    , { gmbcID = "GMBC10.038_880", quality = HighQ, origin = "SAMN02334289" }
    , { gmbcID = "GMBC10.039_017", quality = HighQ, origin = "SAMEA2580212" }
    , { gmbcID = "GMBC10.039_069", quality = MedQ, origin = "SAMEA3708727" }
    , { gmbcID = "GMBC10.039_430", quality = HighQ, origin = "SAMEA3951728" }
    , { gmbcID = "GMBC10.039_457", quality = MedQ, origin = "SAMEA3708643" }
    , { gmbcID = "GMBC10.039_551", quality = MedQ, origin = "SAMN05545037" }
    , { gmbcID = "GMBC10.039_553", quality = MedQ, origin = "SAMN05545036" }
    , { gmbcID = "GMBC10.039_883", quality = HighQ, origin = "SAMEA2467017" }
    , { gmbcID = "GMBC10.040_347", quality = HighQ, origin = "SAMEA2580300" }
    , { gmbcID = "GMBC10.040_573", quality = HighQ, origin = "SAMEA2151383" }
    , { gmbcID = "GMBC10.040_922", quality = HighQ, origin = "SAMEA3387452" }
    , { gmbcID = "GMBC10.040_948", quality = HighQ, origin = "SAMEA2579922" }
    , { gmbcID = "GMBC10.040_968", quality = HighQ, origin = "SAMEA2338799" }
    , { gmbcID = "GMBC10.041_228", quality = HighQ, origin = "SAMEA3708731" }
    , { gmbcID = "GMBC10.041_283", quality = MedQ, origin = "SAMEA3663237" }
    , { gmbcID = "GMBC10.041_364", quality = HighQ, origin = "SAMEA4378286" }
    , { gmbcID = "GMBC10.041_407", quality = HighQ, origin = "SAMEA3708675" }
    , { gmbcID = "GMBC10.041_438", quality = HighQ, origin = "SAMEA2582112" }
    , { gmbcID = "GMBC10.041_457", quality = MedQ, origin = "SAMEA3136678" }
    , { gmbcID = "GMBC10.041_478", quality = HighQ, origin = "SAMN05826585" }
    , { gmbcID = "GMBC10.041_591", quality = HighQ, origin = "SAMEA2737855" }
    , { gmbcID = "GMBC10.041_621", quality = HighQ, origin = "SAMEA3951796" }
    , { gmbcID = "GMBC10.041_788", quality = HighQ, origin = "SAMN00696744" }
    , { gmbcID = "GMBC10.041_809", quality = HighQ, origin = "SAMEA3449363" }
    , { gmbcID = "GMBC10.042_259", quality = HighQ, origin = "SAMEA3708654" }
    , { gmbcID = "GMBC10.042_341", quality = MedQ, origin = "SAMN02676155" }
    , { gmbcID = "GMBC10.042_417", quality = HighQ, origin = "SAMEA2466948" }
    , { gmbcID = "GMBC10.042_464", quality = HighQ, origin = "SAMEA3664625" }
    , { gmbcID = "GMBC10.042_512", quality = HighQ, origin = "ExDog5" }
    , { gmbcID = "GMBC10.042_557", quality = MedQ, origin = "SAMEA2338723" }
    , { gmbcID = "GMBC10.042_697", quality = HighQ, origin = "SAMEA2582137" }
    , { gmbcID = "GMBC10.043_089", quality = HighQ, origin = "SAMEA3663252" }
    , { gmbcID = "GMBC10.043_255", quality = HighQ, origin = "SAMN02676148" }
    , { gmbcID = "GMBC10.043_280", quality = HighQ, origin = "SAMN02334137" }
    , { gmbcID = "GMBC10.043_472", quality = MedQ, origin = "SAMEA3951750" }
    , { gmbcID = "GMBC10.043_636", quality = MedQ, origin = "SAMEA2467076" }
    , { gmbcID = "GMBC10.043_990", quality = HighQ, origin = "SAMEA2580266" }
    , { gmbcID = "GMBC10.044_104", quality = MedQ, origin = "SAMN01780043" }
    , { gmbcID = "GMBC10.044_160", quality = HighQ, origin = "SAMEA2149983" }
    , { gmbcID = "GMBC10.044_187", quality = HighQ, origin = "SAMEA3449421" }
    , { gmbcID = "GMBC10.044_207", quality = HighQ, origin = "SAMEA2737758" }
    , { gmbcID = "GMBC10.044_314", quality = HighQ, origin = "SAMN05826971" }
    , { gmbcID = "GMBC10.044_369", quality = HighQ, origin = "SAMEA2580220" }
    , { gmbcID = "GMBC10.044_487", quality = MedQ, origin = "SAMN05826923" }
    , { gmbcID = "GMBC10.044_525", quality = MedQ, origin = "SAMEA4378320" }
    , { gmbcID = "GMBC10.044_694", quality = HighQ, origin = "SAMN02334043" }
    , { gmbcID = "GMBC10.044_719", quality = HighQ, origin = "SAMEA3136763" }
    , { gmbcID = "GMBC10.044_802", quality = MedQ, origin = "SAMN05827255" }
    , { gmbcID = "GMBC10.044_814", quality = MedQ, origin = "SAMEA3136690" }
    , { gmbcID = "GMBC10.044_822", quality = HighQ, origin = "SAMEA3387444" }
    , { gmbcID = "GMBC10.044_900", quality = MedQ, origin = "SAMEA3449190" }
    , { gmbcID = "GMBC10.044_947", quality = MedQ, origin = "SAMEA3664782" }
    , { gmbcID = "GMBC10.045_052", quality = MedQ, origin = "SAMN04436909" }
    , { gmbcID = "GMBC10.045_065", quality = MedQ, origin = "SAMEA2467048" }
    , { gmbcID = "GMBC10.045_280", quality = HighQ, origin = "SAMEA2467050" }
    , { gmbcID = "GMBC10.045_377", quality = HighQ, origin = "SAMEA3449202" }
    , { gmbcID = "GMBC10.045_447", quality = MedQ, origin = "SAMN02334206" }
    , { gmbcID = "GMBC10.045_558", quality = HighQ, origin = "SAMN04436805" }
    , { gmbcID = "GMBC10.045_814", quality = MedQ, origin = "SAMEA3136689" }
    , { gmbcID = "GMBC10.045_991", quality = HighQ, origin = "SAMN05545090" }
    , { gmbcID = "GMBC10.046_084", quality = HighQ, origin = "SAMEA2467042" }
    , { gmbcID = "GMBC10.046_240", quality = MedQ, origin = "SAMN00779630" }
    , { gmbcID = "GMBC10.046_337", quality = HighQ, origin = "SAMEA4378272" }
    , { gmbcID = "GMBC10.046_525", quality = MedQ, origin = "SAMEA2338669" }
    , { gmbcID = "GMBC10.046_540", quality = MedQ, origin = "SAMN04436774" }
    , { gmbcID = "GMBC10.046_795", quality = HighQ, origin = "SAMN02333918" }
    , { gmbcID = "GMBC10.046_917", quality = HighQ, origin = "SAMN05827227" }
    , { gmbcID = "GMBC10.047_007", quality = HighQ, origin = "SAMN05827230" }
    , { gmbcID = "GMBC10.047_168", quality = HighQ, origin = "SAMN05827263" }
    , { gmbcID = "GMBC10.047_310", quality = HighQ, origin = "SAMEA3708479" }
    , { gmbcID = "GMBC10.047_504", quality = HighQ, origin = "SAMEA3663209" }
    , { gmbcID = "GMBC10.047_540", quality = HighQ, origin = "SAMN02334042" }
    , { gmbcID = "GMBC10.047_604", quality = MedQ, origin = "SAMEA3708556" }
    , { gmbcID = "GMBC10.047_605", quality = MedQ, origin = "SAMEA3708462" }
    , { gmbcID = "GMBC10.047_630", quality = MedQ, origin = "SAMN02333956" }
    , { gmbcID = "GMBC10.047_666", quality = HighQ, origin = "SAMEA2580038" }
    , { gmbcID = "GMBC10.047_982", quality = MedQ, origin = "SAMN02676138" }
    , { gmbcID = "GMBC10.048_209", quality = MedQ, origin = "SAMEA3708593" }
    , { gmbcID = "GMBC10.048_366", quality = MedQ, origin = "SAMN03270129" }
    , { gmbcID = "GMBC10.048_528", quality = HighQ, origin = "SAMEA3136681" }
    , { gmbcID = "GMBC10.048_667", quality = MedQ, origin = "SAMN04436716" }
    , { gmbcID = "GMBC10.048_725", quality = HighQ, origin = "SAMN00991536" }
    , { gmbcID = "GMBC10.048_973", quality = MedQ, origin = "SAMEA3708466" }
    , { gmbcID = "GMBC10.049_010", quality = MedQ, origin = "SAMN02675508" }
    , { gmbcID = "GMBC10.049_095", quality = HighQ, origin = "SAMEA4378268" }
    , { gmbcID = "GMBC10.049_214", quality = MedQ, origin = "SAMEA3951741" }
    , { gmbcID = "GMBC10.049_611", quality = MedQ, origin = "SAMEA3951740" }
    , { gmbcID = "GMBC10.049_772", quality = MedQ, origin = "SAMEA3664634" }
    , { gmbcID = "GMBC10.049_902", quality = MedQ, origin = "SAMN00691429" }
    , { gmbcID = "GMBC10.049_995", quality = MedQ, origin = "SAMEA3449330" }
    , { gmbcID = "GMBC10.050_099", quality = HighQ, origin = "SAMEA2582020" }
    , { gmbcID = "GMBC10.050_182", quality = MedQ, origin = "SAMEA2580029" }
    , { gmbcID = "GMBC10.050_281", quality = MedQ, origin = "SAMEA3541508" }
    , { gmbcID = "GMBC10.050_350", quality = MedQ, origin = "SAMN02675520" }
    , { gmbcID = "GMBC10.050_391", quality = MedQ, origin = "SAMEA2338674" }
    , { gmbcID = "GMBC10.050_545", quality = MedQ, origin = "SAMN00990279" }
    , { gmbcID = "GMBC10.050_623", quality = MedQ, origin = "SAMN04436732" }
    , { gmbcID = "GMBC10.051_045", quality = MedQ, origin = "SAMEA4378289" }
    , { gmbcID = "GMBC10.051_123", quality = HighQ, origin = "SAMEA3136753" }
    , { gmbcID = "GMBC10.051_127", quality = MedQ, origin = "SAMEA2467035" }
    , { gmbcID = "GMBC10.051_220", quality = MedQ, origin = "SAMN01773404" }
    , { gmbcID = "GMBC10.051_239", quality = HighQ, origin = "SAMN00691586" }
    , { gmbcID = "GMBC10.051_287", quality = MedQ, origin = "SAMN01779857" }
    , { gmbcID = "GMBC10.051_288", quality = MedQ, origin = "SAMN01773492" }
    , { gmbcID = "GMBC10.051_383", quality = HighQ, origin = "SAMEA3708510" }
    , { gmbcID = "GMBC10.051_501", quality = MedQ, origin = "SAMEA3449268" }
    , { gmbcID = "GMBC10.051_724", quality = MedQ, origin = "SAMN02676154" }
    , { gmbcID = "GMBC10.051_822", quality = MedQ, origin = "SAMEA3449232" }
    , { gmbcID = "GMBC10.051_962", quality = MedQ, origin = "SAMEA2580290" }
    , { gmbcID = "GMBC10.051_971", quality = MedQ, origin = "SAMEA3664838" }
    , { gmbcID = "GMBC10.051_977", quality = MedQ, origin = "SAMN01779956" }
    , { gmbcID = "GMBC10.052_109", quality = MedQ, origin = "SAMEA2580102" }
    , { gmbcID = "GMBC10.052_276", quality = MedQ, origin = "SAMEA3541529" }
    , { gmbcID = "GMBC10.052_520", quality = MedQ, origin = "SAMN02676164" }
    , { gmbcID = "GMBC10.052_602", quality = MedQ, origin = "SAMN04261267" }
    , { gmbcID = "GMBC10.052_626", quality = MedQ, origin = "ExDog22" }
    , { gmbcID = "GMBC10.053_008", quality = HighQ, origin = "SAMN02333944" }
    , { gmbcID = "GMBC10.053_302", quality = MedQ, origin = "SAMEA3951676" }
    , { gmbcID = "GMBC10.053_357", quality = MedQ, origin = "SAMN01779927" }
    , { gmbcID = "GMBC10.053_436", quality = MedQ, origin = "SAMN03401203" }
    , { gmbcID = "GMBC10.053_530", quality = MedQ, origin = "SAMN05827163" }
    , { gmbcID = "GMBC10.053_551", quality = HighQ, origin = "SAMN01773319" }
    , { gmbcID = "GMBC10.053_685", quality = MedQ, origin = "SAMEA3951659" }
    , { gmbcID = "GMBC10.053_831", quality = MedQ, origin = "SAMEA3136633" }
    , { gmbcID = "GMBC10.053_972", quality = MedQ, origin = "SAMN04436895" }
    , { gmbcID = "GMBC10.054_009", quality = MedQ, origin = "SAMN02334223" }
    , { gmbcID = "GMBC10.054_177", quality = MedQ, origin = "SAMEA3387517" }
    , { gmbcID = "GMBC10.054_207", quality = MedQ, origin = "SAMEA3449253" }
    , { gmbcID = "GMBC10.054_285", quality = MedQ, origin = "SAMEA2580020" }
    , { gmbcID = "GMBC10.054_360", quality = MedQ, origin = "SAMEA3708856" }
    , { gmbcID = "GMBC10.054_624", quality = HighQ, origin = "SAMN02333920" }
    , { gmbcID = "GMBC10.054_819", quality = MedQ, origin = "SAMN04436638" }
    , { gmbcID = "GMBC10.054_847", quality = MedQ, origin = "SAMEA4378282" }
    , { gmbcID = "GMBC10.054_970", quality = MedQ, origin = "SAMN02676165" }
    , { gmbcID = "GMBC10.055_067", quality = MedQ, origin = "SAMEA3541503" }
    , { gmbcID = "GMBC10.055_634", quality = MedQ, origin = "SAMN01779881" }
    , { gmbcID = "GMBC10.055_873", quality = MedQ, origin = "SAMEA3708722" }
    , { gmbcID = "GMBC10.055_955", quality = MedQ, origin = "SAMEA4378257" }
    , { gmbcID = "GMBC10.056_012", quality = MedQ, origin = "SAMEA3449235" }
    , { gmbcID = "GMBC10.056_101", quality = MedQ, origin = "SAMEA3449299" }
    , { gmbcID = "GMBC10.056_177", quality = MedQ, origin = "SAMEA3708630" }
    , { gmbcID = "GMBC10.056_292", quality = MedQ, origin = "SAMEA2158542" }
    , { gmbcID = "GMBC10.056_565", quality = MedQ, origin = "SAMEA3708617" }
    , { gmbcID = "GMBC10.056_596", quality = MedQ, origin = "SAMN03398927" }
    , { gmbcID = "GMBC10.056_607", quality = MedQ, origin = "SAMEA2579978" }
    , { gmbcID = "GMBC10.056_724", quality = HighQ, origin = "SAMN02334181" }
    , { gmbcID = "GMBC10.056_769", quality = MedQ, origin = "SAMEA3951779" }
    , { gmbcID = "GMBC10.056_814", quality = MedQ, origin = "SAMEA3663215" }
    , { gmbcID = "GMBC10.057_148", quality = MedQ, origin = "SAMEA3708646" }
    , { gmbcID = "GMBC10.057_362", quality = MedQ, origin = "SAMEA3136719" }
    , { gmbcID = "GMBC10.057_478", quality = HighQ, origin = "SAMEA3951773" }
    , { gmbcID = "GMBC10.057_513", quality = HighQ, origin = "SAMEA2737663" }
    , { gmbcID = "GMBC10.057_688", quality = MedQ, origin = "SAMEA2737826" }
    , { gmbcID = "GMBC10.057_989", quality = MedQ, origin = "SAMEA3449380" }
    , { gmbcID = "GMBC10.058_205", quality = MedQ, origin = "SAMEA3708902" }
    , { gmbcID = "GMBC10.058_454", quality = MedQ, origin = "SAMEA3387442" }
    , { gmbcID = "GMBC10.058_536", quality = MedQ, origin = "SAMEA2580126" }
    , { gmbcID = "GMBC10.058_717", quality = HighQ, origin = "SAMEA3708518" }
    , { gmbcID = "GMBC10.058_984", quality = MedQ, origin = "SAMN00779899" }
    , { gmbcID = "GMBC10.059_062", quality = MedQ, origin = "SAMEA2580176" }
    , { gmbcID = "GMBC10.059_279", quality = HighQ, origin = "SAMEA4378264" }
    , { gmbcID = "GMBC10.059_490", quality = MedQ, origin = "SAMEA3541582" }
    , { gmbcID = "GMBC10.059_793", quality = MedQ, origin = "SAMEA2737708" }
    , { gmbcID = "GMBC10.059_796", quality = HighQ, origin = "SAMN04436815" }
    , { gmbcID = "GMBC10.060_191", quality = MedQ, origin = "SAMEA2737665" }
    , { gmbcID = "GMBC10.060_350", quality = MedQ, origin = "SAMEA3708616" }
    , { gmbcID = "GMBC10.060_498", quality = MedQ, origin = "SAMN01795789" }
    , { gmbcID = "GMBC10.060_579", quality = MedQ, origin = "SAMN01779907" }
    , { gmbcID = "GMBC10.060_618", quality = MedQ, origin = "SAMEA3708598" }
    , { gmbcID = "GMBC10.060_746", quality = MedQ, origin = "SAMEA2582057" }
    , { gmbcID = "GMBC10.060_842", quality = MedQ, origin = "SAMEA2737728" }
    , { gmbcID = "GMBC10.060_885", quality = MedQ, origin = "SAMEA2466938" }
    , { gmbcID = "GMBC10.061_041", quality = MedQ, origin = "SAMEA2466998" }
    , { gmbcID = "GMBC10.061_237", quality = MedQ, origin = "SAMEA3951736" }
    , { gmbcID = "GMBC10.061_325", quality = HighQ, origin = "SAMN02334076" }
    , { gmbcID = "GMBC10.061_517", quality = MedQ, origin = "SAMEA3449323" }
    , { gmbcID = "GMBC10.061_623", quality = MedQ, origin = "SAMN00699759" }
    , { gmbcID = "GMBC10.061_685", quality = MedQ, origin = "SAMEA3449303" }
    , { gmbcID = "GMBC10.061_781", quality = MedQ, origin = "SAMEA3449276" }
    , { gmbcID = "GMBC10.061_792", quality = MedQ, origin = "SAMEA3708607" }
    , { gmbcID = "GMBC10.061_915", quality = MedQ, origin = "SAMN04436818" }
    , { gmbcID = "GMBC10.061_948", quality = HighQ, origin = "SAMN02675519" }
    , { gmbcID = "GMBC10.061_990", quality = MedQ, origin = "SAMEA3449215" }
    , { gmbcID = "GMBC10.062_083", quality = MedQ, origin = "SAMEA2338712" }
    , { gmbcID = "GMBC10.062_090", quality = MedQ, origin = "SAMEA3134356" }
    , { gmbcID = "GMBC10.062_144", quality = MedQ, origin = "SAMEA3136653" }
    , { gmbcID = "GMBC10.062_162", quality = MedQ, origin = "SAMN02676152" }
    , { gmbcID = "GMBC10.062_569", quality = MedQ, origin = "SAMEA2466986" }
    , { gmbcID = "GMBC10.062_679", quality = MedQ, origin = "SAMN03398624" }
    , { gmbcID = "GMBC10.063_211", quality = MedQ, origin = "SAMN02676182" }
    , { gmbcID = "GMBC10.063_230", quality = MedQ, origin = "SAMN04262593" }
    , { gmbcID = "GMBC10.063_521", quality = MedQ, origin = "SAMN05826926" }
    , { gmbcID = "GMBC10.063_602", quality = MedQ, origin = "SAMN05545018" }
    , { gmbcID = "GMBC10.063_766", quality = MedQ, origin = "SAMEA2737809" }
    , { gmbcID = "GMBC10.063_912", quality = MedQ, origin = "SAMEA3449321" }
    , { gmbcID = "GMBC10.063_965", quality = HighQ, origin = "SAMEA4378292" }
    , { gmbcID = "GMBC10.064_106", quality = MedQ, origin = "SAMEA3663242" }
    , { gmbcID = "GMBC10.064_182", quality = MedQ, origin = "SAMN01780057" }
    , { gmbcID = "GMBC10.064_233", quality = MedQ, origin = "SAMEA3951770" }
    , { gmbcID = "GMBC10.064_246", quality = MedQ, origin = "SAMEA3449158" }
    , { gmbcID = "GMBC10.064_408", quality = MedQ, origin = "SAMEA2580137" }
    , { gmbcID = "GMBC10.064_773", quality = MedQ, origin = "SAMEA3136761" }
    , { gmbcID = "GMBC10.065_046", quality = MedQ, origin = "SAMN03401178" }
    , { gmbcID = "GMBC10.065_616", quality = MedQ, origin = "SAMEA3951628" }
    , { gmbcID = "GMBC10.066_058", quality = MedQ, origin = "SAMEA4378270" }
    , { gmbcID = "GMBC10.066_088", quality = MedQ, origin = "SAMEA3449350" }
    , { gmbcID = "GMBC10.066_160", quality = MedQ, origin = "SRX1670030" }
    , { gmbcID = "GMBC10.066_485", quality = MedQ, origin = "SAMEA3951756" }
    , { gmbcID = "GMBC10.066_556", quality = MedQ, origin = "SAMEA3664682" }
    , { gmbcID = "GMBC10.066_579", quality = MedQ, origin = "SAMEA3951746" }
    , { gmbcID = "GMBC10.066_873", quality = MedQ, origin = "SAMEA2144223" }
    , { gmbcID = "GMBC10.067_102", quality = MedQ, origin = "SAMN02334089" }
    , { gmbcID = "GMBC10.067_132", quality = MedQ, origin = "SAMEA2042112" }
    , { gmbcID = "GMBC10.067_417", quality = MedQ, origin = "SAMN04261317" }
    , { gmbcID = "GMBC10.067_527", quality = MedQ, origin = "SAMEA3449326" }
    , { gmbcID = "GMBC10.067_864", quality = HighQ, origin = "SAMEA3664817" }
    , { gmbcID = "GMBC10.068_357", quality = MedQ, origin = "SAMEA3663271" }
    , { gmbcID = "GMBC10.068_437", quality = HighQ, origin = "SAMN02676203" }
    , { gmbcID = "GMBC10.068_449", quality = MedQ, origin = "SAMEA3708778" }
    , { gmbcID = "GMBC10.068_466", quality = HighQ, origin = "SAMEA4378314" }
    , { gmbcID = "GMBC10.068_473", quality = MedQ, origin = "SAMEA3136707" }
    , { gmbcID = "GMBC10.068_494", quality = MedQ, origin = "SAMEA2737685" }
    , { gmbcID = "GMBC10.068_580", quality = MedQ, origin = "SAMEA3449320" }
    , { gmbcID = "GMBC10.068_631", quality = MedQ, origin = "SAMEA3136683" }
    , { gmbcID = "GMBC10.068_702", quality = MedQ, origin = "SAMEA3449183" }
    , { gmbcID = "GMBC10.068_993", quality = MedQ, origin = "SAMEA4378296" }
    , { gmbcID = "GMBC10.069_121", quality = MedQ, origin = "SAMN02333929" }
    , { gmbcID = "GMBC10.069_238", quality = MedQ, origin = "SAMEA3951696" }
    , { gmbcID = "GMBC10.069_253", quality = MedQ, origin = "SAMN02676217" }
    , { gmbcID = "GMBC10.069_453", quality = MedQ, origin = "SAMEA2737783" }
    , { gmbcID = "GMBC10.069_537", quality = MedQ, origin = "SAMEA3708896" }
    , { gmbcID = "GMBC10.069_622", quality = MedQ, origin = "SAMEA3664569" }
    , { gmbcID = "GMBC10.069_629", quality = MedQ, origin = "SAMEA3541563" }
    , { gmbcID = "GMBC10.069_634", quality = MedQ, origin = "SAMEA3449377" }
    , { gmbcID = "GMBC10.069_701", quality = MedQ, origin = "SAMN00699746" }
    , { gmbcID = "GMBC10.069_896", quality = MedQ, origin = "SAMEA3708716" }
    , { gmbcID = "GMBC10.069_908", quality = MedQ, origin = "SAMN02334182" }
    , { gmbcID = "GMBC10.070_114", quality = MedQ, origin = "SAMEA2737650" }
    , { gmbcID = "GMBC10.070_375", quality = MedQ, origin = "SAMEA2582038" }
    , { gmbcID = "GMBC10.070_404", quality = MedQ, origin = "SAMEA2582117" }
    , { gmbcID = "GMBC10.070_466", quality = MedQ, origin = "SAMN03398940" }
    , { gmbcID = "GMBC10.070_627", quality = MedQ, origin = "SAMEA3708837" }
    , { gmbcID = "GMBC10.070_867", quality = MedQ, origin = "SAMEA2338652" }
    , { gmbcID = "GMBC10.071_005", quality = MedQ, origin = "SAMEA2580034" }
    , { gmbcID = "GMBC10.071_055", quality = MedQ, origin = "SAMEA3664969" }
    , { gmbcID = "GMBC10.071_114", quality = MedQ, origin = "SAMN02676186" }
    , { gmbcID = "GMBC10.071_130", quality = MedQ, origin = "SAMN04436910" }
    , { gmbcID = "GMBC10.071_267", quality = MedQ, origin = "SRS016585" }
    , { gmbcID = "GMBC10.071_509", quality = MedQ, origin = "SAMEA3664545" }
    , { gmbcID = "GMBC10.071_576", quality = MedQ, origin = "SAMEA3449156" }
    , { gmbcID = "GMBC10.072_058", quality = MedQ, origin = "SAMEA3708662" }
    , { gmbcID = "GMBC10.072_690", quality = MedQ, origin = "SAMEA3449374" }
    , { gmbcID = "GMBC10.072_976", quality = MedQ, origin = "SAMN03398862" }
    , { gmbcID = "GMBC10.073_000", quality = MedQ, origin = "SAMEA3708832" }
    , { gmbcID = "GMBC10.073_108", quality = MedQ, origin = "SAMEA3449267" }
    , { gmbcID = "GMBC10.073_134", quality = MedQ, origin = "SAMEA2579910" }
    , { gmbcID = "GMBC10.073_200", quality = MedQ, origin = "SAMN03398849" }
    , { gmbcID = "GMBC10.073_347", quality = MedQ, origin = "SAMEA2580094" }
    , { gmbcID = "GMBC10.073_378", quality = MedQ, origin = "SAMEA4378319" }
    , { gmbcID = "GMBC10.073_461", quality = MedQ, origin = "SAMN04261273" }
    , { gmbcID = "GMBC10.073_555", quality = MedQ, origin = "SAMEA4378327" }
    , { gmbcID = "GMBC10.073_698", quality = MedQ, origin = "SAMN01774043" }
    , { gmbcID = "GMBC10.073_937", quality = MedQ, origin = "SAMN04436846" }
    , { gmbcID = "GMBC10.074_095", quality = MedQ, origin = "SAMEA2582131" }
    , { gmbcID = "GMBC10.074_176", quality = MedQ, origin = "SAMEA2338653" }
    , { gmbcID = "GMBC10.074_346", quality = MedQ, origin = "SAMN04436790" }
    , { gmbcID = "GMBC10.074_423", quality = MedQ, origin = "SAMEA3541591" }
    , { gmbcID = "GMBC10.074_562", quality = MedQ, origin = "SAMN01773357" }
    , { gmbcID = "GMBC10.074_741", quality = MedQ, origin = "SAMEA2582086" }
    , { gmbcID = "GMBC10.074_766", quality = MedQ, origin = "SAMEA3449228" }
    , { gmbcID = "GMBC10.074_925", quality = MedQ, origin = "SAMN04436597" }
    , { gmbcID = "GMBC10.075_020", quality = MedQ, origin = "SAMEA2580255" }
    , { gmbcID = "GMBC10.075_162", quality = MedQ, origin = "SAMN02676200" }
    , { gmbcID = "GMBC10.075_181", quality = MedQ, origin = "SAMN05827135" }
    , { gmbcID = "GMBC10.075_701", quality = MedQ, origin = "SAMN02676132" }
    , { gmbcID = "GMBC10.075_997", quality = MedQ, origin = "SAMEA2338769" }
    , { gmbcID = "GMBC10.076_030", quality = HighQ, origin = "SAMEA3708703" }
    , { gmbcID = "GMBC10.076_109", quality = MedQ, origin = "SAMN01773349" }
    , { gmbcID = "GMBC10.076_312", quality = MedQ, origin = "SAMEA3951643" }
    , { gmbcID = "GMBC10.076_345", quality = MedQ, origin = "SAMEA2580238" }
    , { gmbcID = "GMBC10.076_477", quality = MedQ, origin = "SAMEA2582120" }
    , { gmbcID = "GMBC10.076_527", quality = MedQ, origin = "SAMEA3449184" }
    , { gmbcID = "GMBC10.076_551", quality = MedQ, origin = "SAMEA3951718" }
    , { gmbcID = "GMBC10.076_947", quality = MedQ, origin = "SAMEA2467033" }
    , { gmbcID = "GMBC10.077_006", quality = MedQ, origin = "SAMEA3663280" }
    , { gmbcID = "GMBC10.077_178", quality = MedQ, origin = "SAMN02676149" }
    , { gmbcID = "GMBC10.077_266", quality = HighQ, origin = "SAMN00990340" }
    , { gmbcID = "GMBC10.077_306", quality = MedQ, origin = "SAMEA2338855" }
    , { gmbcID = "GMBC10.077_692", quality = MedQ, origin = "SAMEA3708527" }
    , { gmbcID = "GMBC10.077_736", quality = MedQ, origin = "SAMN01795792" }
    , { gmbcID = "GMBC10.077_922", quality = MedQ, origin = "SAMEA2580130" }
    , { gmbcID = "GMBC10.078_037", quality = MedQ, origin = "SAMN04262501" }
    , { gmbcID = "GMBC10.078_191", quality = MedQ, origin = "SAMEA3708678" }
    , { gmbcID = "GMBC10.078_239", quality = MedQ, origin = "SAMEA3663012" }
    , { gmbcID = "GMBC10.078_344", quality = MedQ, origin = "SAMN04436857" }
    , { gmbcID = "GMBC10.078_353", quality = MedQ, origin = "SAMEA2737653" }
    , { gmbcID = "GMBC10.078_364", quality = MedQ, origin = "SAMN02333965" }
    , { gmbcID = "GMBC10.078_494", quality = MedQ, origin = "SAMEA1906486" }
    , { gmbcID = "GMBC10.078_740", quality = MedQ, origin = "SAMN01779884" }
    , { gmbcID = "GMBC10.079_062", quality = MedQ, origin = "SAMEA2580152" }
    , { gmbcID = "GMBC10.079_071", quality = MedQ, origin = "SAMN05826569" }
    , { gmbcID = "GMBC10.079_156", quality = MedQ, origin = "SAMEA3951767" }
    , { gmbcID = "GMBC10.079_334", quality = MedQ, origin = "SAMEA3951698" }
    , { gmbcID = "GMBC10.079_633", quality = MedQ, origin = "SAMN05827122" }
    , { gmbcID = "GMBC10.079_650", quality = MedQ, origin = "SAMN02676146" }
    , { gmbcID = "GMBC10.079_757", quality = MedQ, origin = "SAMEA2467004" }
    , { gmbcID = "GMBC10.080_085", quality = MedQ, origin = "SAMEA2580108" }
    , { gmbcID = "GMBC10.080_417", quality = MedQ, origin = "SAMEA3708492" }
    , { gmbcID = "GMBC10.080_527", quality = MedQ, origin = "SAMEA3449173" }
    , { gmbcID = "GMBC10.080_900", quality = MedQ, origin = "SAMEA2580271" }
    , { gmbcID = "GMBC10.080_904", quality = MedQ, origin = "SAMN05544994" }
    , { gmbcID = "GMBC10.080_924", quality = MedQ, origin = "SAMN04261335" }
    , { gmbcID = "GMBC10.081_181", quality = MedQ, origin = "SAMEA3708714" }
    , { gmbcID = "GMBC10.081_213", quality = MedQ, origin = "SAMEA2338680" }
    , { gmbcID = "GMBC10.081_429", quality = MedQ, origin = "SAMEA3708872" }
    , { gmbcID = "GMBC10.081_451", quality = MedQ, origin = "SAMN05544991" }
    , { gmbcID = "GMBC10.081_687", quality = MedQ, origin = "SAMEA3708487" }
    , { gmbcID = "GMBC10.081_838", quality = MedQ, origin = "SAMEA3951727" }
    , { gmbcID = "GMBC10.081_987", quality = MedQ, origin = "SAMEA2466955" }
    , { gmbcID = "GMBC10.082_153", quality = MedQ, origin = "SAMN03270059" }
    , { gmbcID = "GMBC10.082_238", quality = MedQ, origin = "SAMEA2582107" }
    , { gmbcID = "GMBC10.082_243", quality = MedQ, origin = "SAMN00699742" }
    , { gmbcID = "GMBC10.082_397", quality = MedQ, origin = "SAMN01774008" }
    , { gmbcID = "GMBC10.082_583", quality = MedQ, origin = "SAMEA2338690" }
    , { gmbcID = "GMBC10.082_831", quality = MedQ, origin = "SAMN00696746" }
    , { gmbcID = "GMBC10.083_008", quality = MedQ, origin = "SAMEA3951685" }
    , { gmbcID = "GMBC10.083_133", quality = MedQ, origin = "SAMN04436856" }
    , { gmbcID = "GMBC10.083_248", quality = MedQ, origin = "SAMN04261218" }
    , { gmbcID = "GMBC10.083_271", quality = MedQ, origin = "SAMEA3708537" }
    , { gmbcID = "GMBC10.083_568", quality = MedQ, origin = "SAMEA2737692" }
    , { gmbcID = "GMBC10.083_739", quality = MedQ, origin = "SAMEA3708850" }
    , { gmbcID = "GMBC10.083_821", quality = MedQ, origin = "SAMN05545009" }
    , { gmbcID = "GMBC10.083_874", quality = MedQ, origin = "SAMEA2338665" }
    , { gmbcID = "GMBC10.083_886", quality = MedQ, origin = "SAMN01795937" }
    , { gmbcID = "GMBC10.083_887", quality = MedQ, origin = "SAMN00779901" }
    , { gmbcID = "GMBC10.083_938", quality = MedQ, origin = "SAMEA3541518" }
    , { gmbcID = "GMBC10.083_939", quality = MedQ, origin = "SAMEA3449383" }
    , { gmbcID = "GMBC10.084_164", quality = MedQ, origin = "SAMN04261339" }
    , { gmbcID = "GMBC10.084_327", quality = MedQ, origin = "SAMEA3664643" }
    , { gmbcID = "GMBC10.084_502", quality = MedQ, origin = "SAMEA2737757" }
    , { gmbcID = "GMBC10.084_512", quality = MedQ, origin = "SAMEA3951720" }
    , { gmbcID = "GMBC10.084_857", quality = MedQ, origin = "SAMEA3951768" }
    , { gmbcID = "GMBC10.084_922", quality = MedQ, origin = "SAMEA3449340" }
    , { gmbcID = "GMBC10.084_975", quality = MedQ, origin = "SAMN01773502" }
    , { gmbcID = "GMBC10.085_022", quality = MedQ, origin = "SAMEA2737823" }
    , { gmbcID = "GMBC10.085_041", quality = MedQ, origin = "SAMEA3708781" }
    , { gmbcID = "GMBC10.085_156", quality = MedQ, origin = "SAMN03398763" }
    , { gmbcID = "GMBC10.085_324", quality = MedQ, origin = "SAMEA2581911" }
    , { gmbcID = "GMBC10.085_480", quality = MedQ, origin = "SAMN03398796" }
    , { gmbcID = "GMBC10.085_544", quality = MedQ, origin = "SAMN01795803" }
    , { gmbcID = "GMBC10.085_594", quality = MedQ, origin = "ExDogA27" }
    , { gmbcID = "GMBC10.085_648", quality = MedQ, origin = "SAMEA3541522" }
    , { gmbcID = "GMBC10.085_651", quality = MedQ, origin = "SAMEA2737847" }
    , { gmbcID = "GMBC10.086_056", quality = MedQ, origin = "ExDogA11" }
    , { gmbcID = "GMBC10.086_220", quality = MedQ, origin = "SAMEA2579936" }
    , { gmbcID = "GMBC10.086_634", quality = MedQ, origin = "SAMN04261349" }
    , { gmbcID = "GMBC10.087_005", quality = MedQ, origin = "SAMN05827178" }
    , { gmbcID = "GMBC10.087_050", quality = MedQ, origin = "SAMN03401283" }
    , { gmbcID = "GMBC10.087_243", quality = MedQ, origin = "SAMN02676133" }
    , { gmbcID = "GMBC10.087_245", quality = MedQ, origin = "SAMEA3541516" }
    , { gmbcID = "GMBC10.087_563", quality = MedQ, origin = "SAMEA2737878" }
    , { gmbcID = "GMBC10.087_777", quality = MedQ, origin = "SAMN02404522" }
    , { gmbcID = "GMBC10.087_890", quality = MedQ, origin = "SAMEA2737790" }
    , { gmbcID = "GMBC10.088_133", quality = MedQ, origin = "SAMN01774283" }
    , { gmbcID = "GMBC10.088_231", quality = MedQ, origin = "SAMEA2580153" }
    , { gmbcID = "GMBC10.088_279", quality = MedQ, origin = "SAMN00001760" }
    , { gmbcID = "GMBC10.088_332", quality = MedQ, origin = "SAMN01906337" }
    , { gmbcID = "GMBC10.088_369", quality = MedQ, origin = "SAMEA3665035" }
    , { gmbcID = "GMBC10.088_385", quality = MedQ, origin = "SAMEA3387497" }
    , { gmbcID = "GMBC10.088_394", quality = MedQ, origin = "SAMEA2338809" }
    , { gmbcID = "GMBC10.088_549", quality = MedQ, origin = "SAMN01773343" }
    , { gmbcID = "GMBC10.089_063", quality = MedQ, origin = "SAMEA2338845" }
    , { gmbcID = "GMBC10.089_105", quality = MedQ, origin = "SAMN05826822" }
    , { gmbcID = "GMBC10.089_294", quality = MedQ, origin = "SAMN02334067" }
    , { gmbcID = "GMBC10.089_512", quality = MedQ, origin = "SAMEA3541478" }
    , { gmbcID = "GMBC10.089_580", quality = MedQ, origin = "SAMN01779952" }
    , { gmbcID = "GMBC10.089_611", quality = MedQ, origin = "SAMEA2737745" }
    , { gmbcID = "GMBC10.089_651", quality = MedQ, origin = "SAMEA2338734" }
    , { gmbcID = "GMBC10.089_864", quality = MedQ, origin = "SAMN02675501" }
    , { gmbcID = "GMBC10.089_953", quality = MedQ, origin = "SAMEA2737711" }
    , { gmbcID = "GMBC10.090_062", quality = MedQ, origin = "SAMEA1967055" }
    , { gmbcID = "GMBC10.090_310", quality = MedQ, origin = "SAMEA3708483" }
    , { gmbcID = "GMBC10.090_379", quality = MedQ, origin = "SAMEA2737739" }
    , { gmbcID = "GMBC10.090_653", quality = MedQ, origin = "SAMEA2338624" }
    , { gmbcID = "GMBC10.090_680", quality = MedQ, origin = "SAMEA3708849" }
    , { gmbcID = "GMBC10.090_889", quality = MedQ, origin = "SAMEA3708536" }
    , { gmbcID = "GMBC10.090_912", quality = MedQ, origin = "SAMN02676171" }
    , { gmbcID = "GMBC10.091_099", quality = MedQ, origin = "SAMN02676210" }
    , { gmbcID = "GMBC10.091_265", quality = MedQ, origin = "SAMN02676183" }
    , { gmbcID = "GMBC10.091_321", quality = MedQ, origin = "SAMEA3663013" }
    , { gmbcID = "GMBC10.091_383", quality = MedQ, origin = "SAMN02334084" }
    , { gmbcID = "GMBC10.091_548", quality = MedQ, origin = "SAMEA3541589" }
    , { gmbcID = "GMBC10.091_696", quality = MedQ, origin = "SAMEA2338702" }
    , { gmbcID = "GMBC10.092_006", quality = MedQ, origin = "SAMN01795781" }
    , { gmbcID = "GMBC10.092_097", quality = MedQ, origin = "SAMEA2579961" }
    , { gmbcID = "GMBC10.092_201", quality = MedQ, origin = "SAMN02333943" }
    , { gmbcID = "GMBC10.092_366", quality = MedQ, origin = "SAMEA3708543" }
    , { gmbcID = "GMBC10.092_375", quality = MedQ, origin = "SAMEA3449325" }
    , { gmbcID = "GMBC10.092_383", quality = MedQ, origin = "SAMEA2580074" }
    , { gmbcID = "GMBC10.092_471", quality = MedQ, origin = "SAMN02334134" }
    , { gmbcID = "GMBC10.092_482", quality = MedQ, origin = "SAMN01795864" }
    , { gmbcID = "GMBC10.092_484", quality = MedQ, origin = "SAMN01773456" }
    , { gmbcID = "GMBC10.092_485", quality = MedQ, origin = "SAMEA4378329" }
    , { gmbcID = "GMBC10.092_514", quality = MedQ, origin = "SAMEA3708506" }
    , { gmbcID = "GMBC10.092_745", quality = MedQ, origin = "SAMN05826486" }
    , { gmbcID = "GMBC10.092_799", quality = MedQ, origin = "SAMN05545060" }
    , { gmbcID = "GMBC10.093_215", quality = MedQ, origin = "SAMEA3663023" }
    , { gmbcID = "GMBC10.093_235", quality = MedQ, origin = "SAMEA3449370" }
    , { gmbcID = "GMBC10.093_300", quality = MedQ, origin = "SAMEA1906490" }
    , { gmbcID = "GMBC10.093_332", quality = MedQ, origin = "SAMEA2580030" }
    , { gmbcID = "GMBC10.093_398", quality = MedQ, origin = "SAMEA2582136" }
    , { gmbcID = "GMBC10.093_413", quality = MedQ, origin = "SAMEA2737752" }
    , { gmbcID = "GMBC10.093_926", quality = MedQ, origin = "SAMEA2338735" }
    , { gmbcID = "GMBC10.093_979", quality = MedQ, origin = "SAMN04436753" }
    , { gmbcID = "GMBC10.094_062", quality = MedQ, origin = "SAMEA3951784" }
    , { gmbcID = "GMBC10.094_098", quality = MedQ, origin = "SAMEA4378309" }
    , { gmbcID = "GMBC10.094_155", quality = MedQ, origin = "SAMEA3879520" }
    , { gmbcID = "GMBC10.094_186", quality = MedQ, origin = "SAMEA3951792" }
    , { gmbcID = "GMBC10.094_237", quality = MedQ, origin = "SAMN04436777" }
    , { gmbcID = "GMBC10.094_341", quality = MedQ, origin = "SAMN05827243" }
    , { gmbcID = "GMBC10.094_372", quality = MedQ, origin = "SAMN01780023" }
    , { gmbcID = "GMBC10.094_839", quality = MedQ, origin = "SAMEA3387435" }
    , { gmbcID = "GMBC10.094_890", quality = MedQ, origin = "SAMEA3708650" }
    , { gmbcID = "GMBC10.095_197", quality = MedQ, origin = "SAMN02676172" }
    , { gmbcID = "GMBC10.095_317", quality = MedQ, origin = "SAMEA2582128" }
    , { gmbcID = "GMBC10.095_353", quality = MedQ, origin = "SAMN02676193" }
    , { gmbcID = "GMBC10.095_446", quality = MedQ, origin = "SAMEA3136627" }
    , { gmbcID = "GMBC10.095_591", quality = MedQ, origin = "SAMN05545083" }
    , { gmbcID = "GMBC10.095_629", quality = MedQ, origin = "SAMN01112480" }
    , { gmbcID = "GMBC10.095_652", quality = MedQ, origin = "SAMEA3708779" }
    , { gmbcID = "GMBC10.095_686", quality = MedQ, origin = "SAMEA3449313" }
    , { gmbcID = "GMBC10.095_779", quality = MedQ, origin = "SAMEA3708535" }
    , { gmbcID = "GMBC10.096_498", quality = MedQ, origin = "SAMN05545041" }
    , { gmbcID = "GMBC10.096_696", quality = MedQ, origin = "SAMEA3708494" }
    , { gmbcID = "GMBC10.096_851", quality = MedQ, origin = "SAMN02334115" }
    , { gmbcID = "GMBC10.096_925", quality = MedQ, origin = "SAMEA2338812" }
    , { gmbcID = "GMBC10.096_930", quality = MedQ, origin = "SAMEA3708883" }
    , { gmbcID = "GMBC10.097_211", quality = MedQ, origin = "SAMEA4378299" }
    , { gmbcID = "GMBC10.097_305", quality = MedQ, origin = "SAMN00990286" }
    , { gmbcID = "GMBC10.097_444", quality = MedQ, origin = "SAMEA3951789" }
    , { gmbcID = "GMBC10.097_889", quality = MedQ, origin = "SAMN03399975" }
    , { gmbcID = "GMBC10.098_030", quality = MedQ, origin = "SAMEA3387457" }
    , { gmbcID = "GMBC10.098_149", quality = MedQ, origin = "SAMEA3449168" }
    , { gmbcID = "GMBC10.098_192", quality = MedQ, origin = "SAMEA3879575" }
    , { gmbcID = "GMBC10.098_637", quality = MedQ, origin = "SAMN02676168" }
    , { gmbcID = "GMBC10.098_647", quality = MedQ, origin = "SAMEA3449240" }
    , { gmbcID = "GMBC10.098_890", quality = MedQ, origin = "SAMEA3136765" }
    , { gmbcID = "GMBC10.098_962", quality = MedQ, origin = "ExDog45" }
    , { gmbcID = "GMBC10.098_978", quality = MedQ, origin = "SAMEA3708520" }
    , { gmbcID = "GMBC10.099_034", quality = MedQ, origin = "SAMEA3951708" }
    , { gmbcID = "GMBC10.099_146", quality = MedQ, origin = "SAMEA3708830" }
    , { gmbcID = "GMBC10.099_317", quality = MedQ, origin = "SAMEA2466949" }
    , { gmbcID = "GMBC10.099_567", quality = MedQ, origin = "SAMN05826776" }
    , { gmbcID = "GMBC10.099_835", quality = MedQ, origin = "SAMEA3708755" }
    , { gmbcID = "GMBC10.099_840", quality = MedQ, origin = "SAMEA2580302" }
    , { gmbcID = "GMBC10.100_347", quality = MedQ, origin = "SAMEA3708657" }
    , { gmbcID = "GMBC10.100_371", quality = MedQ, origin = "SAMN03398949" }
    , { gmbcID = "GMBC10.100_458", quality = MedQ, origin = "SAMN02333982" }
    , { gmbcID = "GMBC10.100_616", quality = MedQ, origin = "SAMEA4378291" }
    , { gmbcID = "GMBC10.100_660", quality = MedQ, origin = "SAMN01773338" }
    , { gmbcID = "GMBC10.100_707", quality = MedQ, origin = "SAMN04261381" }
    , { gmbcID = "GMBC10.100_762", quality = MedQ, origin = "SAMEA3708501" }
    , { gmbcID = "GMBC10.100_789", quality = MedQ, origin = "SAMN05545028" }
    , { gmbcID = "GMBC10.101_107", quality = MedQ, origin = "SAMN02676169" }
    , { gmbcID = "GMBC10.101_303", quality = MedQ, origin = "SAMN02675507" }
    , { gmbcID = "GMBC10.101_703", quality = MedQ, origin = "SAMEA3708481" }
    , { gmbcID = "GMBC10.101_897", quality = MedQ, origin = "SAMEA3708499" }
    , { gmbcID = "GMBC10.101_916", quality = MedQ, origin = "SAMEA2580180" }
    , { gmbcID = "GMBC10.102_045", quality = MedQ, origin = "SAMEA4378324" }
    , { gmbcID = "GMBC10.102_193", quality = MedQ, origin = "SAMN02676147" }
    , { gmbcID = "GMBC10.102_294", quality = MedQ, origin = "SAMN02334037" }
    , { gmbcID = "GMBC10.102_401", quality = MedQ, origin = "SAMEA3708645" }
    , { gmbcID = "GMBC10.102_565", quality = MedQ, origin = "SAMEA3708805" }
    , { gmbcID = "GMBC10.103_257", quality = MedQ, origin = "SAMEA2338732" }
    , { gmbcID = "GMBC10.103_582", quality = MedQ, origin = "SAMN01780004" }
    , { gmbcID = "GMBC10.103_599", quality = MedQ, origin = "SAMEA3449270" }
    , { gmbcID = "GMBC10.103_734", quality = MedQ, origin = "SAMN02676187" }
    , { gmbcID = "GMBC10.103_765", quality = MedQ, origin = "SAMN01780298" }
    , { gmbcID = "GMBC10.104_563", quality = MedQ, origin = "SAMN01795812" }
    , { gmbcID = "GMBC10.104_608", quality = MedQ, origin = "SAMEA3708475" }
    , { gmbcID = "GMBC10.104_651", quality = MedQ, origin = "SAMEA2338792" }
    , { gmbcID = "GMBC10.104_769", quality = MedQ, origin = "SAMEA2580209" }
    , { gmbcID = "GMBC10.104_889", quality = MedQ, origin = "SAMN02333966" }
    , { gmbcID = "GMBC10.105_040", quality = MedQ, origin = "SAMN02675503" }
    , { gmbcID = "GMBC10.105_056", quality = MedQ, origin = "SAMN00001761" }
    , { gmbcID = "GMBC10.105_176", quality = MedQ, origin = "SAMEA2580047" }
    , { gmbcID = "GMBC10.105_178", quality = MedQ, origin = "SAMEA2466922" }
    , { gmbcID = "GMBC10.105_334", quality = MedQ, origin = "SAMEA3708720" }
    , { gmbcID = "GMBC10.105_399", quality = MedQ, origin = "SAMN01795962" }
    , { gmbcID = "GMBC10.105_536", quality = MedQ, origin = "SAMEA2580208" }
    , { gmbcID = "GMBC10.105_543", quality = MedQ, origin = "SAMN02676130" }
    , { gmbcID = "GMBC10.105_641", quality = MedQ, origin = "SAMN02676166" }
    , { gmbcID = "GMBC10.106_154", quality = MedQ, origin = "SAMEA2467016" }
    , { gmbcID = "GMBC10.106_658", quality = MedQ, origin = "SAMN05826884" }
    , { gmbcID = "GMBC10.106_662", quality = MedQ, origin = "SAMN04436807" }
    , { gmbcID = "GMBC10.106_920", quality = MedQ, origin = "SAMEA3664668" }
    , { gmbcID = "GMBC10.107_262", quality = MedQ, origin = "SAMN04436759" }
    , { gmbcID = "GMBC10.107_622", quality = MedQ, origin = "SAMEA2580254" }
    , { gmbcID = "GMBC10.108_032", quality = MedQ, origin = "SAMEA3708826" }
    , { gmbcID = "GMBC10.108_146", quality = MedQ, origin = "SAMN05545000" }
    , { gmbcID = "GMBC10.108_173", quality = MedQ, origin = "SAMN02333979" }
    , { gmbcID = "GMBC10.108_387", quality = MedQ, origin = "ExDog18" }
    , { gmbcID = "GMBC10.108_572", quality = MedQ, origin = "SAMN00001766" }
    , { gmbcID = "GMBC10.108_677", quality = MedQ, origin = "SAMEA3708773" }
    , { gmbcID = "GMBC10.108_915", quality = MedQ, origin = "SAMN02676116" }
    , { gmbcID = "GMBC10.108_932", quality = MedQ, origin = "SAMEA3449242" }
    , { gmbcID = "GMBC10.109_144", quality = MedQ, origin = "SAMN01041728" }
    , { gmbcID = "GMBC10.109_311", quality = MedQ, origin = "SAMEA3708884" }
    , { gmbcID = "GMBC10.109_492", quality = MedQ, origin = "SAMN04261274" }
    , { gmbcID = "GMBC10.109_499", quality = MedQ, origin = "SAMN02676150" }
    , { gmbcID = "GMBC10.109_887", quality = MedQ, origin = "SAMEA4378294" }
    , { gmbcID = "GMBC10.110_117", quality = MedQ, origin = "SAMEA2338684" }
    , { gmbcID = "GMBC10.110_422", quality = MedQ, origin = "SAMEA3951662" }
    , { gmbcID = "GMBC10.110_468", quality = MedQ, origin = "SAMN05545077" }
    , { gmbcID = "GMBC10.110_727", quality = MedQ, origin = "SAMEA3449291" }
    , { gmbcID = "GMBC10.111_285", quality = LowQ, origin = "SAMEA3708901" }
    , { gmbcID = "GMBC10.111_305", quality = LowQ, origin = "SAMEA3708680" }
    , { gmbcID = "GMBC10.111_415", quality = MedQ, origin = "SAMEA2737846" }
    , { gmbcID = "GMBC10.111_418", quality = LowQ, origin = "SAMEA2737800" }
    , { gmbcID = "GMBC10.111_460", quality = MedQ, origin = "SAMEA2338682" }
    , { gmbcID = "GMBC10.111_598", quality = MedQ, origin = "SAMEA3708688" }
    , { gmbcID = "GMBC10.111_759", quality = LowQ, origin = "SAMEA3449384" }
    , { gmbcID = "GMBC10.111_956", quality = LowQ, origin = "SAMEA3443758" }
    , { gmbcID = "GMBC10.112_003", quality = MedQ, origin = "SAMEA3708519" }
    , { gmbcID = "GMBC10.112_289", quality = MedQ, origin = "SAMN05827259" }
    , { gmbcID = "GMBC10.112_373", quality = MedQ, origin = "SAMN02676195" }
    , { gmbcID = "GMBC10.112_642", quality = LowQ, origin = "SAMN01779844" }
    , { gmbcID = "GMBC10.112_663", quality = MedQ, origin = "SAMEA2580125" }
    , { gmbcID = "GMBC10.113_093", quality = MedQ, origin = "ExDog34" }
    , { gmbcID = "GMBC10.113_497", quality = LowQ, origin = "SAMN02676167" }
    , { gmbcID = "GMBC10.113_695", quality = LowQ, origin = "SAMEA3663245" }
    , { gmbcID = "GMBC10.113_714", quality = LowQ, origin = "SAMEA3708509" }
    , { gmbcID = "GMBC10.113_847", quality = MedQ, origin = "SAMEA2467030" }
    , { gmbcID = "GMBC10.113_874", quality = MedQ, origin = "SAMEA3136665" }
    , { gmbcID = "GMBC10.114_150", quality = MedQ, origin = "SAMN00691167" }
    , { gmbcID = "GMBC10.114_230", quality = LowQ, origin = "SAMN03401296" }
    , { gmbcID = "GMBC10.114_245", quality = LowQ, origin = "SAMN02675511" }
    , { gmbcID = "GMBC10.114_252", quality = LowQ, origin = "SAMN02334153" }
    , { gmbcID = "GMBC10.114_390", quality = LowQ, origin = "SAMEA2737861" }
    , { gmbcID = "GMBC10.114_412", quality = LowQ, origin = "SAMEA2580147" }
    , { gmbcID = "GMBC10.114_496", quality = MedQ, origin = "SAMN05545032" }
    , { gmbcID = "GMBC10.114_574", quality = MedQ, origin = "SAMEA2737719" }
    , { gmbcID = "GMBC10.114_775", quality = LowQ, origin = "SAMN01041728" }
    , { gmbcID = "GMBC10.115_046", quality = LowQ, origin = "SAMN02676139" }
    , { gmbcID = "GMBC10.115_276", quality = MedQ, origin = "SAMEA2153898" }
    , { gmbcID = "GMBC10.115_685", quality = LowQ, origin = "SAMEA3387500" }
    , { gmbcID = "GMBC10.116_090", quality = LowQ, origin = "SAMN01780040" }
    , { gmbcID = "GMBC10.116_287", quality = MedQ, origin = "SAMEA2338637" }
    , { gmbcID = "GMBC10.116_484", quality = MedQ, origin = "SAMN02676199" }
    , { gmbcID = "GMBC10.116_683", quality = MedQ, origin = "ExDog14" }
    , { gmbcID = "GMBC10.116_746", quality = MedQ, origin = "SAMEA2737651" }
    , { gmbcID = "GMBC10.116_879", quality = LowQ, origin = "SAMEA3951635" }
    , { gmbcID = "GMBC10.117_134", quality = LowQ, origin = "SAMEA3664928" }
    , { gmbcID = "GMBC10.117_234", quality = MedQ, origin = "SAMN04436966" }
    , { gmbcID = "GMBC10.117_342", quality = LowQ, origin = "SAMN04436735" }
    , { gmbcID = "GMBC10.117_359", quality = LowQ, origin = "SAMN02334149" }
    , { gmbcID = "GMBC10.117_371", quality = LowQ, origin = "SAMN01795880" }
    , { gmbcID = "GMBC10.117_484", quality = LowQ, origin = "SAMEA3449233" }
    , { gmbcID = "GMBC10.117_553", quality = LowQ, origin = "SAMEA2579929" }
    , { gmbcID = "GMBC10.117_783", quality = LowQ, origin = "SAMN01779844" }
    , { gmbcID = "GMBC10.117_784", quality = LowQ, origin = "SAMN01773301" }
    , { gmbcID = "GMBC10.117_806", quality = LowQ, origin = "SAMEA2737686" }
    , { gmbcID = "GMBC10.117_809", quality = LowQ, origin = "ExDog44" }
    , { gmbcID = "GMBC10.117_811", quality = LowQ, origin = "ExDog37" }
    , { gmbcID = "GMBC10.117_935", quality = LowQ, origin = "ExDog8" }
    , { gmbcID = "GMBC10.118_287", quality = LowQ, origin = "SAMN01774012" }
    , { gmbcID = "GMBC10.118_374", quality = LowQ, origin = "SAMN02334054" }
    , { gmbcID = "GMBC10.118_473", quality = MedQ, origin = "SAMEA2580292" }
    , { gmbcID = "GMBC10.118_564", quality = LowQ, origin = "SAMN01774320" }
    , { gmbcID = "GMBC10.118_639", quality = MedQ, origin = "SAMN02675517" }
    , { gmbcID = "GMBC10.118_682", quality = LowQ, origin = "SAMEA2581933" }
    , { gmbcID = "GMBC10.118_834", quality = LowQ, origin = "SAMN04261242" }
    , { gmbcID = "GMBC10.118_893", quality = LowQ, origin = "SAMEA3708748" }
    , { gmbcID = "GMBC10.118_974", quality = MedQ, origin = "SAMEA3951629" }
    , { gmbcID = "GMBC10.119_087", quality = LowQ, origin = "SAMEA2580133" }
    , { gmbcID = "GMBC10.119_478", quality = LowQ, origin = "SAMEA3449174" }
    , { gmbcID = "GMBC10.119_650", quality = LowQ, origin = "SAMN02676136" }
    , { gmbcID = "GMBC10.120_098", quality = LowQ, origin = "SAMN04436855" }
    , { gmbcID = "GMBC10.120_192", quality = LowQ, origin = "SAMN02676131" }
    , { gmbcID = "GMBC10.120_241", quality = MedQ, origin = "SAMEA3708581" }
    , { gmbcID = "GMBC10.120_697", quality = LowQ, origin = "SAMN02333981" }
    , { gmbcID = "GMBC10.121_144", quality = LowQ, origin = "SAMEA2338859" }
    , { gmbcID = "GMBC10.121_621", quality = LowQ, origin = "SAMEA3449252" }
    , { gmbcID = "GMBC10.121_647", quality = LowQ, origin = "SAMN00691414" }
    , { gmbcID = "GMBC10.121_657", quality = LowQ, origin = "SAMN02334016" }
    , { gmbcID = "GMBC10.121_994", quality = LowQ, origin = "SAMEA3541547" }
    , { gmbcID = "GMBC10.122_096", quality = LowQ, origin = "SAMEA3664878" }
    , { gmbcID = "GMBC10.122_110", quality = MedQ, origin = "SAMEA3541588" }
    , { gmbcID = "GMBC10.122_128", quality = LowQ, origin = "SAMEA3541531" }
    , { gmbcID = "GMBC10.122_145", quality = LowQ, origin = "SAMN01780020" }
    , { gmbcID = "GMBC10.122_367", quality = MedQ, origin = "SAMEA3708548" }
    , { gmbcID = "GMBC10.122_776", quality = LowQ, origin = "SAMEA3449427" }
    , { gmbcID = "GMBC10.122_828", quality = LowQ, origin = "SAMN05545010" }
    , { gmbcID = "GMBC10.122_830", quality = LowQ, origin = "SAMN01779870" }
    , { gmbcID = "GMBC10.122_836", quality = LowQ, origin = "SAMEA3136722" }
    , { gmbcID = "GMBC10.123_006", quality = LowQ, origin = "SAMEA2579917" }
    , { gmbcID = "GMBC10.123_252", quality = LowQ, origin = "SAMN01774317" }
    , { gmbcID = "GMBC10.123_645", quality = LowQ, origin = "SAMN01780047" }
    , { gmbcID = "GMBC10.123_656", quality = LowQ, origin = "SAMEA2737763" }
    , { gmbcID = "GMBC10.123_787", quality = LowQ, origin = "SAMN02334290" }
    , { gmbcID = "GMBC10.124_049", quality = LowQ, origin = "SAMN01889407" }
    , { gmbcID = "GMBC10.124_053", quality = LowQ, origin = "SAMN01773462" }
    , { gmbcID = "GMBC10.124_190", quality = LowQ, origin = "SAMEA3136652" }
    , { gmbcID = "GMBC10.124_394", quality = LowQ, origin = "SAMN02676153" }
    , { gmbcID = "GMBC10.124_721", quality = LowQ, origin = "SAMN05826491" }
    , { gmbcID = "GMBC10.124_870", quality = MedQ, origin = "SAMEA4378248" }
    , { gmbcID = "GMBC10.124_982", quality = LowQ, origin = "SAMEA2580104" }
    , { gmbcID = "GMBC10.125_060", quality = LowQ, origin = "SAMN01773406" }
    , { gmbcID = "GMBC10.125_093", quality = LowQ, origin = "SAMEA2579935" }
    , { gmbcID = "GMBC10.125_222", quality = LowQ, origin = "SAMEA3708601" }
    , { gmbcID = "GMBC10.125_224", quality = LowQ, origin = "SAMEA3136722" }
    , { gmbcID = "GMBC10.125_692", quality = LowQ, origin = "SAMN04261369" }
    , { gmbcID = "GMBC10.125_817", quality = MedQ, origin = "SAMEA3708467" }
    , { gmbcID = "GMBC10.125_845", quality = LowQ, origin = "SAMN05827141" }
    , { gmbcID = "GMBC10.125_944", quality = LowQ, origin = "SAMN01985131" }
    , { gmbcID = "GMBC10.126_277", quality = LowQ, origin = "SAMN01773456" }
    , { gmbcID = "GMBC10.126_436", quality = LowQ, origin = "SAMN01795855" }
    , { gmbcID = "GMBC10.126_444", quality = LowQ, origin = "SAMEA1906560" }
    , { gmbcID = "GMBC10.126_558", quality = LowQ, origin = "SAMEA3664943" }
    , { gmbcID = "GMBC10.126_586", quality = LowQ, origin = "SAMEA2338733" }
    , { gmbcID = "GMBC10.126_628", quality = LowQ, origin = "SAMN05826891" }
    , { gmbcID = "GMBC10.126_808", quality = LowQ, origin = "SAMEA3708733" }
    , { gmbcID = "GMBC10.126_923", quality = LowQ, origin = "SAMEA4378273" }
    , { gmbcID = "GMBC10.127_165", quality = LowQ, origin = "SAMEA4378266" }
    , { gmbcID = "GMBC10.127_185", quality = LowQ, origin = "SAMEA3449277" }
    , { gmbcID = "GMBC10.127_382", quality = LowQ, origin = "SAMEA4378313" }
    , { gmbcID = "GMBC10.127_528", quality = LowQ, origin = "SAMEA3541521" }
    , { gmbcID = "GMBC10.128_291", quality = LowQ, origin = "SAMN02334202" }
    , { gmbcID = "GMBC10.128_354", quality = LowQ, origin = "SAMEA3708640" }
    , { gmbcID = "GMBC10.128_688", quality = LowQ, origin = "SAMN02334040" }
    , { gmbcID = "GMBC10.128_742", quality = LowQ, origin = "SAMEA3708721" }
    , { gmbcID = "GMBC10.128_823", quality = LowQ, origin = "SAMEA2338689" }
    , { gmbcID = "GMBC10.128_840", quality = LowQ, origin = "SAMEA2737677" }
    , { gmbcID = "GMBC10.129_043", quality = LowQ, origin = "SAMEA3449295" }
    , { gmbcID = "GMBC10.129_108", quality = MedQ, origin = "SAMN02676196" }
    , { gmbcID = "GMBC10.129_324", quality = LowQ, origin = "SAMEA3708689" }
    , { gmbcID = "GMBC10.129_629", quality = LowQ, origin = "SAMEA2466984" }
    , { gmbcID = "GMBC10.130_298", quality = LowQ, origin = "SAMEA3708538" }
    , { gmbcID = "GMBC10.130_643", quality = LowQ, origin = "SRS017191" }
    , { gmbcID = "GMBC10.130_675", quality = LowQ, origin = "SAMN05545010" }
    , { gmbcID = "GMBC10.130_685", quality = LowQ, origin = "SAMN03398852" }
    , { gmbcID = "GMBC10.130_733", quality = LowQ, origin = "SAMN01779870" }
    , { gmbcID = "GMBC10.130_736", quality = LowQ, origin = "SAMN00691395" }
    , { gmbcID = "GMBC10.130_790", quality = LowQ, origin = "SAMEA3708733" }
    , { gmbcID = "GMBC10.130_817", quality = LowQ, origin = "SAMEA3664928" }
    , { gmbcID = "GMBC10.130_978", quality = LowQ, origin = "SAMEA2338733" }
    , { gmbcID = "GMBC10.131_191", quality = LowQ, origin = "SAMEA3951772" }
    , { gmbcID = "GMBC10.131_271", quality = LowQ, origin = "SAMN00696735" }
    , { gmbcID = "GMBC10.131_438", quality = LowQ, origin = "SAMN05826785" }
    , { gmbcID = "GMBC10.131_540", quality = LowQ, origin = "SAMEA2338854" }
    , { gmbcID = "GMBC10.131_580", quality = LowQ, origin = "SAMN05545038" }
    , { gmbcID = "GMBC10.132_265", quality = LowQ, origin = "SAMEA3449319" }
    , { gmbcID = "GMBC10.132_298", quality = LowQ, origin = "SAMEA4378259" }
    , { gmbcID = "GMBC10.132_335", quality = LowQ, origin = "SAMN05545087" }
    , { gmbcID = "GMBC10.133_099", quality = LowQ, origin = "SAMEA2580147" }
    , { gmbcID = "GMBC10.133_496", quality = LowQ, origin = "SAMEA3708468" }
    , { gmbcID = "GMBC10.133_658", quality = LowQ, origin = "SAMN05545024" }
    , { gmbcID = "GMBC10.133_994", quality = LowQ, origin = "SAMN05826625" }
    , { gmbcID = "GMBC10.134_069", quality = LowQ, origin = "SAMN01780040" }
    , { gmbcID = "GMBC10.134_238", quality = LowQ, origin = "SAMEA3449209" }
    , { gmbcID = "GMBC10.134_242", quality = LowQ, origin = "SAMEA3449174" }
    , { gmbcID = "GMBC10.134_250", quality = LowQ, origin = "SAMEA3136767" }
    , { gmbcID = "GMBC10.134_393", quality = LowQ, origin = "ExDog8" }
    , { gmbcID = "GMBC10.134_454", quality = LowQ, origin = "SAMEA2737861" }
    , { gmbcID = "GMBC10.134_965", quality = LowQ, origin = "SAMEA3136741" }
    , { gmbcID = "GMBC10.135_285", quality = LowQ, origin = "SAMN02675499" }
    , { gmbcID = "GMBC10.135_314", quality = LowQ, origin = "SAMEA2466966" }
    , { gmbcID = "GMBC10.135_321", quality = LowQ, origin = "SAMEA3449370" }
    , { gmbcID = "GMBC10.135_426", quality = LowQ, origin = "SAMEA2582129" }
    , { gmbcID = "GMBC10.135_512", quality = LowQ, origin = "SAMEA2467024" }
    , { gmbcID = "GMBC10.135_687", quality = MedQ, origin = "SAMEA3951763" }
    , { gmbcID = "GMBC10.136_327", quality = LowQ, origin = "SAMN05827180" }
    , { gmbcID = "GMBC10.136_437", quality = LowQ, origin = "SAMEA3664684" }
    , { gmbcID = "GMBC10.136_622", quality = LowQ, origin = "SAMEA3449376" }
    , { gmbcID = "GMBC10.136_795", quality = LowQ, origin = "SAMN05826651" }
    , { gmbcID = "GMBC10.136_927", quality = LowQ, origin = "SAMEA2579963" }
    , { gmbcID = "GMBC10.136_928", quality = LowQ, origin = "SAMEA2466913" }
    , { gmbcID = "GMBC10.137_164", quality = LowQ, origin = "SAMEA3708681" }
    , { gmbcID = "GMBC10.137_233", quality = MedQ, origin = "SAMEA2579931" }
    , { gmbcID = "GMBC10.137_438", quality = LowQ, origin = "SAMN02334016" }
    , { gmbcID = "GMBC10.137_444", quality = LowQ, origin = "SAMN01779982" }
    , { gmbcID = "GMBC10.137_446", quality = LowQ, origin = "SAMN01774111" }
    , { gmbcID = "GMBC10.137_447", quality = LowQ, origin = "SAMN01773301" }
    , { gmbcID = "GMBC10.137_452", quality = LowQ, origin = "SAMN00690842" }
    , { gmbcID = "GMBC10.137_463", quality = LowQ, origin = "SAMEA3951701" }
    , { gmbcID = "GMBC10.137_514", quality = LowQ, origin = "SAMEA3708688" }
    , { gmbcID = "GMBC10.137_676", quality = LowQ, origin = "SAMEA2737811" }
    , { gmbcID = "GMBC10.138_326", quality = LowQ, origin = "SAMEA2467013" }
    , { gmbcID = "GMBC10.138_349", quality = LowQ, origin = "SAMEA3136767" }
    , { gmbcID = "GMBC10.138_549", quality = LowQ, origin = "SAMN05827198" }
    , { gmbcID = "GMBC10.138_667", quality = LowQ, origin = "SAMN04436735" }
    , { gmbcID = "GMBC10.138_938", quality = LowQ, origin = "SAMN04261323" }
    , { gmbcID = "GMBC10.138_971", quality = LowQ, origin = "SAMEA2737725" }
    , { gmbcID = "GMBC10.139_097", quality = LowQ, origin = "SRS012273" }
    , { gmbcID = "GMBC10.139_181", quality = LowQ, origin = "SAMEA2582043" }
    , { gmbcID = "GMBC10.139_227", quality = LowQ, origin = "SAMN02333994" }
    , { gmbcID = "GMBC10.139_234", quality = LowQ, origin = "SAMEA2737767" }
    , { gmbcID = "GMBC10.139_413", quality = LowQ, origin = "SAMEA3541521" }
    , { gmbcID = "GMBC10.139_450", quality = LowQ, origin = "SAMN01779952" }
    , { gmbcID = "GMBC10.139_493", quality = LowQ, origin = "SAMN01779982" }
    , { gmbcID = "GMBC10.139_866", quality = LowQ, origin = "SAMEA2338735" }
    , { gmbcID = "GMBC10.140_132", quality = LowQ, origin = "SAMN02675511" }
    , { gmbcID = "GMBC10.140_235", quality = LowQ, origin = "SAMEA3136652" }
    , { gmbcID = "GMBC10.140_411", quality = LowQ, origin = "SAMN04436807" }
    , { gmbcID = "GMBC10.140_419", quality = LowQ, origin = "SAMEA3708849" }
    , { gmbcID = "GMBC10.140_452", quality = LowQ, origin = "SAMEA3449310" }
    , { gmbcID = "GMBC10.140_794", quality = LowQ, origin = "SAMN02676142" }
    , { gmbcID = "GMBC10.140_799", quality = LowQ, origin = "SAMN02675517" }
    , { gmbcID = "GMBC10.140_825", quality = LowQ, origin = "SAMN01774012" }
    , { gmbcID = "GMBC10.140_968", quality = LowQ, origin = "SAMEA3449332" }
    , { gmbcID = "GMBC10.140_977", quality = LowQ, origin = "SAMEA3449209" }
    , { gmbcID = "GMBC10.141_061", quality = LowQ, origin = "SAMEA2737686" }
    , { gmbcID = "GMBC10.141_082", quality = LowQ, origin = "SAMEA2580146" }
    , { gmbcID = "GMBC10.141_498", quality = LowQ, origin = "SAMEA3136765" }
    , { gmbcID = "GMBC10.141_507", quality = LowQ, origin = "SAMEA2580090" }
    , { gmbcID = "GMBC10.141_767", quality = LowQ, origin = "SAMEA3449181" }
    , { gmbcID = "GMBC10.141_774", quality = LowQ, origin = "SAMEA3663278" }
    , { gmbcID = "GMBC10.141_788", quality = LowQ, origin = "SAMN02675497" }
    , { gmbcID = "GMBC10.141_842", quality = LowQ, origin = "SAMEA3136726" }
    , { gmbcID = "GMBC10.142_104", quality = LowQ, origin = "SAMEA3449151" }
    , { gmbcID = "GMBC10.142_198", quality = MedQ, origin = "SAMN00769058" }
    , { gmbcID = "GMBC10.142_260", quality = LowQ, origin = "SAMN01773310" }
    , { gmbcID = "GMBC10.142_395", quality = LowQ, origin = "SAMEA3449308" }
    , { gmbcID = "GMBC10.142_495", quality = LowQ, origin = "SAMEA3541549" }
    , { gmbcID = "GMBC10.142_525", quality = LowQ, origin = "SAMEA2737785" }
    , { gmbcID = "GMBC10.143_196", quality = LowQ, origin = "SAMN02334134" }
    , { gmbcID = "GMBC10.143_407", quality = LowQ, origin = "SAMEA2737852" }
    , { gmbcID = "GMBC10.143_462", quality = LowQ, origin = "SAMN02675514" }
    , { gmbcID = "GMBC10.143_597", quality = LowQ, origin = "SAMN02676151" }
    , { gmbcID = "GMBC10.143_852", quality = LowQ, origin = "SAMEA2466957" }
    , { gmbcID = "GMBC10.143_858", quality = LowQ, origin = "SAMEA3708677" }
    , { gmbcID = "GMBC10.143_973", quality = LowQ, origin = "SAMN01773462" }
    , { gmbcID = "GMBC10.144_161", quality = LowQ, origin = "SAMN02675514" }
    , { gmbcID = "GMBC10.144_199", quality = LowQ, origin = "SAMN01985131" }
    , { gmbcID = "GMBC10.144_298", quality = LowQ, origin = "SAMEA3708500" }
    , { gmbcID = "GMBC10.144_778", quality = LowQ, origin = "SAMEA4378271" }
    , { gmbcID = "GMBC10.144_875", quality = LowQ, origin = "SAMEA2531406" }
    , { gmbcID = "GMBC10.145_583", quality = LowQ, origin = "SAMEA3665052" }
    , { gmbcID = "GMBC10.145_661", quality = LowQ, origin = "SAMN02676131" }
    , { gmbcID = "GMBC10.145_841", quality = LowQ, origin = "SAMEA3708551" }
    , { gmbcID = "GMBC10.146_176", quality = LowQ, origin = "SAMEA3449331" }
    , { gmbcID = "GMBC10.146_356", quality = LowQ, origin = "SAMEA3387459" }
    , { gmbcID = "GMBC10.146_465", quality = LowQ, origin = "SAMEA2580265" }
    , { gmbcID = "GMBC10.146_506", quality = LowQ, origin = "SAMN05827132" }
    , { gmbcID = "GMBC10.146_864", quality = LowQ, origin = "SAMEA3708680" }
    , { gmbcID = "GMBC10.146_921", quality = LowQ, origin = "SAMN04436789" }
    , { gmbcID = "GMBC10.147_136", quality = LowQ, origin = "SAMEA3708586" }
    , { gmbcID = "GMBC10.147_685", quality = LowQ, origin = "SAMN02675505" }
    , { gmbcID = "GMBC10.147_712", quality = LowQ, origin = "SAMN01780020" }
    , { gmbcID = "GMBC10.147_715", quality = LowQ, origin = "SAMN01774320" }
    , { gmbcID = "GMBC10.147_716", quality = LowQ, origin = "SAMN01774111" }
    , { gmbcID = "GMBC10.147_743", quality = LowQ, origin = "SAMEA3951632" }
    , { gmbcID = "GMBC10.147_782", quality = LowQ, origin = "SAMEA3708774" }
    , { gmbcID = "GMBC10.147_936", quality = LowQ, origin = "SAMEA3449252" }
    , { gmbcID = "GMBC10.147_943", quality = LowQ, origin = "SAMEA3449168" }
    , { gmbcID = "GMBC10.148_178", quality = LowQ, origin = "SAMN02334095" }
    , { gmbcID = "GMBC10.148_245", quality = LowQ, origin = "SAMEA3449305" }
    , { gmbcID = "GMBC10.148_284", quality = LowQ, origin = "SAMEA3708506" }
    , { gmbcID = "GMBC10.148_436", quality = LowQ, origin = "SAMEA1906533" }
    , { gmbcID = "GMBC10.149_110", quality = LowQ, origin = "SAMEA3708779" }
    , { gmbcID = "GMBC10.149_273", quality = LowQ, origin = "SAMEA3663040" }
    , { gmbcID = "GMBC10.149_792", quality = LowQ, origin = "SAMEA3879507" }
    , { gmbcID = "GMBC10.149_870", quality = LowQ, origin = "SAMEA3708465" }
    , { gmbcID = "GMBC10.150_382", quality = LowQ, origin = "SAMEA3708901" }
    , { gmbcID = "GMBC10.150_673", quality = LowQ, origin = "SAMEA3664504" }
    , { gmbcID = "GMBC10.150_816", quality = LowQ, origin = "SAMEA3708783" }
    , { gmbcID = "GMBC10.151_025", quality = LowQ, origin = "SAMN02675497" }
    , { gmbcID = "GMBC10.151_287", quality = LowQ, origin = "SAMN05545038" }
    , { gmbcID = "GMBC10.151_368", quality = LowQ, origin = "SAMN02334004" }
    , { gmbcID = "GMBC10.151_390", quality = LowQ, origin = "SAMN01780023" }
    , { gmbcID = "GMBC10.151_394", quality = LowQ, origin = "SAMN01773343" }
    , { gmbcID = "GMBC10.151_466", quality = LowQ, origin = "SAMEA3708677" }
    , { gmbcID = "GMBC10.151_476", quality = LowQ, origin = "SAMEA3708607" }
    , { gmbcID = "GMBC10.151_565", quality = LowQ, origin = "SAMEA3541531" }
    , { gmbcID = "GMBC10.151_571", quality = LowQ, origin = "SAMEA3449376" }
    , { gmbcID = "GMBC10.151_744", quality = LowQ, origin = "SAMEA2467013" }
    , { gmbcID = "GMBC10.151_759", quality = LowQ, origin = "SAMEA2338732" }
    , { gmbcID = "GMBC10.151_763", quality = LowQ, origin = "SAMEA2338711" }
    , { gmbcID = "GMBC10.152_161", quality = LowQ, origin = "SAMEA2338676" }
    , { gmbcID = "GMBC10.152_253", quality = LowQ, origin = "SAMN02676146" }
    , { gmbcID = "GMBC10.152_259", quality = LowQ, origin = "ExDog18" }
    , { gmbcID = "GMBC10.152_548", quality = LowQ, origin = "SAMEA2580047" }
    , { gmbcID = "GMBC10.152_735", quality = LowQ, origin = "SAMEA3708526" }
    , { gmbcID = "GMBC10.152_878", quality = LowQ, origin = "SAMEA3449374" }
    , { gmbcID = "GMBC10.153_173", quality = LowQ, origin = "SAMEA2737773" }
    , { gmbcID = "GMBC10.153_255", quality = LowQ, origin = "SAMEA3664710" }
    , { gmbcID = "GMBC10.153_569", quality = LowQ, origin = "SAMN02675499" }
    , { gmbcID = "GMBC10.153_600", quality = LowQ, origin = "SAMEA3708694" }
    , { gmbcID = "GMBC10.154_223", quality = LowQ, origin = "SAMN01774043" }
    , { gmbcID = "GMBC10.154_277", quality = LowQ, origin = "SAMN02334148" }
    , { gmbcID = "GMBC10.154_346", quality = LowQ, origin = "SAMEA2580050" }
    , { gmbcID = "GMBC10.154_382", quality = LowQ, origin = "SAMN00690846" }
    , { gmbcID = "GMBC10.154_805", quality = LowQ, origin = "SAMN02334027" }
    , { gmbcID = "GMBC10.155_018", quality = LowQ, origin = "SAMN04436770" }
    , { gmbcID = "GMBC10.155_113", quality = LowQ, origin = "SAMN02334001" }
    , { gmbcID = "GMBC10.155_161", quality = LowQ, origin = "SAMEA3951688" }
    , { gmbcID = "GMBC10.155_268", quality = LowQ, origin = "SAMEA3665035" }
    , { gmbcID = "GMBC10.155_326", quality = LowQ, origin = "SAMEA3663253" }
    , { gmbcID = "GMBC10.155_378", quality = LowQ, origin = "SAMEA3449310" }
    , { gmbcID = "GMBC10.155_495", quality = LowQ, origin = "SAMEA2737852" }
    , { gmbcID = "GMBC10.156_486", quality = LowQ, origin = "SAMN01774024" }
    , { gmbcID = "GMBC10.156_666", quality = LowQ, origin = "SAMEA3708891" }
    , { gmbcID = "GMBC10.156_701", quality = LowQ, origin = "SAMN02334093" }
    , { gmbcID = "GMBC10.156_752", quality = LowQ, origin = "SAMEA3879561" }
    , { gmbcID = "GMBC10.156_859", quality = MedQ, origin = "SAMEA3951785" }
    , { gmbcID = "GMBC10.156_871", quality = LowQ, origin = "SAMEA2466952" }
    , { gmbcID = "GMBC10.157_156", quality = LowQ, origin = "SAMEA3951642" }
    , { gmbcID = "GMBC10.157_335", quality = LowQ, origin = "SAMN05827186" }
    , { gmbcID = "GMBC10.157_514", quality = LowQ, origin = "SAMEA3708582" }
    , { gmbcID = "GMBC10.158_503", quality = LowQ, origin = "SAMEA3708767" }
    , { gmbcID = "GMBC10.158_697", quality = LowQ, origin = "SAMEA3136683" }
    , { gmbcID = "GMBC10.159_011", quality = LowQ, origin = "SAMN05545083" }
    , { gmbcID = "GMBC10.159_027", quality = LowQ, origin = "SAMN05545000" }
    , { gmbcID = "GMBC10.159_038", quality = LowQ, origin = "SAMN04261275" }
    , { gmbcID = "GMBC10.159_080", quality = LowQ, origin = "SAMN02676142" }
    , { gmbcID = "GMBC10.159_155", quality = LowQ, origin = "SAMN01773416" }
    , { gmbcID = "GMBC10.159_156", quality = LowQ, origin = "SAMN01773313" }
    , { gmbcID = "GMBC10.159_158", quality = LowQ, origin = "SAMN00699759" }
    , { gmbcID = "GMBC10.159_295", quality = LowQ, origin = "SAMEA3708571" }
    , { gmbcID = "GMBC10.159_423", quality = LowQ, origin = "SAMEA3541591" }
    , { gmbcID = "GMBC10.159_444", quality = LowQ, origin = "SAMEA3449384" }
    , { gmbcID = "GMBC10.159_446", quality = LowQ, origin = "SAMEA3449383" }
    , { gmbcID = "GMBC10.159_470", quality = LowQ, origin = "SAMEA3449277" }
    , { gmbcID = "GMBC10.159_756", quality = LowQ, origin = "SAMEA2338854" }
    , { gmbcID = "GMBC10.159_821", quality = LowQ, origin = "SAMEA1906422" }
    , { gmbcID = "GMBC10.160_106", quality = LowQ, origin = "SAMEA3449156" }
    , { gmbcID = "GMBC10.160_331", quality = LowQ, origin = "SAMEA2466917" }
    , { gmbcID = "GMBC10.160_601", quality = LowQ, origin = "SAMN03399971" }
    , { gmbcID = "GMBC10.160_930", quality = LowQ, origin = "SAMN02334053" }
    , { gmbcID = "GMBC10.160_960", quality = LowQ, origin = "SAMEA3541518" }
    , { gmbcID = "GMBC10.161_054", quality = LowQ, origin = "SAMEA3449183" }
    , { gmbcID = "GMBC10.161_085", quality = LowQ, origin = "SAMEA3449308" }
    , { gmbcID = "GMBC10.161_171", quality = LowQ, origin = "SAMEA2737725" }
    , { gmbcID = "GMBC10.161_243", quality = LowQ, origin = "SAMN04261358" }
    , { gmbcID = "GMBC10.161_276", quality = LowQ, origin = "SAMN02333925" }
    , { gmbcID = "GMBC10.161_333", quality = LowQ, origin = "SAMEA3663256" }
    , { gmbcID = "GMBC10.161_584", quality = LowQ, origin = "SAMN02334051" }
    , { gmbcID = "GMBC10.161_871", quality = LowQ, origin = "SAMN02676193" }
    , { gmbcID = "GMBC10.161_908", quality = LowQ, origin = "SAMEA3708691" }
    , { gmbcID = "GMBC10.162_946", quality = LowQ, origin = "SAMEA2467021" }
    , { gmbcID = "GMBC10.163_232", quality = LowQ, origin = "SAMN02676137" }
    , { gmbcID = "GMBC10.163_318", quality = LowQ, origin = "SAMEA3951745" }
    , { gmbcID = "GMBC10.163_324", quality = LowQ, origin = "SAMEA3951694" }
    , { gmbcID = "GMBC10.163_924", quality = LowQ, origin = "ExDog1" }
    , { gmbcID = "GMBC10.164_259", quality = LowQ, origin = "SAMEA3708498" }
    , { gmbcID = "GMBC10.164_849", quality = LowQ, origin = "SAMN05827162" }
    , { gmbcID = "GMBC10.165_024", quality = LowQ, origin = "SAMN05826927" }
    , { gmbcID = "GMBC10.165_199", quality = LowQ, origin = "SAMEA3708612" }
    , { gmbcID = "GMBC10.165_583", quality = LowQ, origin = "SAMN05827124" }
    , { gmbcID = "GMBC10.165_595", quality = LowQ, origin = "SAMEA3449233" }
    , { gmbcID = "GMBC10.165_666", quality = LowQ, origin = "SAMN00690842" }
    , { gmbcID = "GMBC10.165_788", quality = LowQ, origin = "SAMEA1906433" }
    , { gmbcID = "GMBC10.165_792", quality = LowQ, origin = "SAMEA3951633" }
    , { gmbcID = "GMBC10.166_351", quality = LowQ, origin = "SAMN05826960" }
    , { gmbcID = "GMBC10.166_399", quality = LowQ, origin = "SAMEA3449313" }
    , { gmbcID = "GMBC10.166_643", quality = MedQ, origin = "SAMN05827143" }
    , { gmbcID = "GMBC10.167_020", quality = LowQ, origin = "SAMN04436789" }
    , { gmbcID = "GMBC10.167_077", quality = LowQ, origin = "SAMN02676137" }
    , { gmbcID = "GMBC10.167_139", quality = LowQ, origin = "SAMN01774317" }
    , { gmbcID = "GMBC10.167_140", quality = LowQ, origin = "SAMN01773416" }
    , { gmbcID = "GMBC10.167_143", quality = LowQ, origin = "SAMN01773338" }
    , { gmbcID = "GMBC10.167_557", quality = LowQ, origin = "SAMEA2737764" }
    , { gmbcID = "GMBC10.167_595", quality = LowQ, origin = "SAMEA2582013" }
    , { gmbcID = "GMBC10.167_838", quality = LowQ, origin = "SAMEA2579912" }
    , { gmbcID = "GMBC10.167_876", quality = LowQ, origin = "SAMEA3708500" }
    , { gmbcID = "GMBC10.169_027", quality = LowQ, origin = "SAMN04436710" }
    , { gmbcID = "GMBC10.169_228", quality = LowQ, origin = "SAMN02676195" }
    , { gmbcID = "GMBC10.169_301", quality = LowQ, origin = "SAMN01795864" }
    , { gmbcID = "GMBC10.169_394", quality = LowQ, origin = "SAMN03401222" }
    , { gmbcID = "GMBC10.169_659", quality = LowQ, origin = "SAMEA3449215" }
    , { gmbcID = "GMBC10.169_807", quality = LowQ, origin = "SAMEA2149068" }
    , { gmbcID = "GMBC10.169_875", quality = LowQ, origin = "SAMN04436789" }
    , { gmbcID = "GMBC10.170_506", quality = LowQ, origin = "SAMEA4378279" }
    , { gmbcID = "GMBC10.170_578", quality = LowQ, origin = "SAMEA3951766" }
    , { gmbcID = "GMBC10.170_713", quality = LowQ, origin = "SAMN04436770" }
    , { gmbcID = "GMBC10.171_187", quality = LowQ, origin = "SAMN03270129" }
    , { gmbcID = "GMBC10.171_274", quality = LowQ, origin = "SAMN01889407" }
    , { gmbcID = "GMBC10.171_281", quality = LowQ, origin = "SAMN01773313" }
    , { gmbcID = "GMBC10.171_282", quality = LowQ, origin = "SAMN01773313" }
    , { gmbcID = "GMBC10.171_565", quality = LowQ, origin = "SAMEA3449296" }
    , { gmbcID = "GMBC10.171_768", quality = LowQ, origin = "SAMEA2582159" }
    , { gmbcID = "GMBC10.171_833", quality = LowQ, origin = "SAMEA2466915" }
    , { gmbcID = "GMBC10.173_261", quality = LowQ, origin = "SAMN03398738" }
    , { gmbcID = "GMBC10.173_872", quality = LowQ, origin = "SAMN00691408" }
    , { gmbcID = "GMBC10.174_533", quality = LowQ, origin = "SAMN02334149" }
    , { gmbcID = "GMBC10.174_598", quality = LowQ, origin = "SAMN04261267" }
    , { gmbcID = "GMBC10.174_837", quality = LowQ, origin = "SAMN02675507" }
    , { gmbcID = "GMBC10.175_180", quality = LowQ, origin = "SAMN05827119" }
    , { gmbcID = "GMBC10.175_199", quality = LowQ, origin = "SAMN05545076" }
    , { gmbcID = "GMBC10.175_207", quality = LowQ, origin = "SAMN05545039" }
    , { gmbcID = "GMBC10.175_208", quality = LowQ, origin = "SAMN05545037" }
    , { gmbcID = "GMBC10.175_210", quality = LowQ, origin = "SAMN05545036" }
    , { gmbcID = "GMBC10.175_290", quality = LowQ, origin = "SAMN02676137" }
    , { gmbcID = "GMBC10.175_373", quality = LowQ, origin = "SAMN01773416" }
    , { gmbcID = "GMBC10.175_374", quality = LowQ, origin = "SAMN01773406" }
    , { gmbcID = "GMBC10.175_379", quality = LowQ, origin = "SAMN00691395" }
    , { gmbcID = "GMBC10.175_398", quality = LowQ, origin = "SAMEA4378249" }
    , { gmbcID = "GMBC10.175_510", quality = LowQ, origin = "SAMEA3708636" }
    , { gmbcID = "GMBC10.175_870", quality = LowQ, origin = "SAMEA2737799" }
    , { gmbcID = "GMBC10.175_998", quality = LowQ, origin = "SAMEA2466938" }
    , { gmbcID = "GMBC10.176_560", quality = LowQ, origin = "SAMEA2581888" }
    , { gmbcID = "GMBC10.177_176", quality = LowQ, origin = "SAMEA3664800" }
    , { gmbcID = "GMBC10.177_213", quality = LowQ, origin = "SAMN05827153" }
    , { gmbcID = "GMBC10.177_561", quality = LowQ, origin = "SAMEA3663275" }
    , { gmbcID = "GMBC10.177_879", quality = LowQ, origin = "SAMN00696746" }
    , { gmbcID = "GMBC10.178_151", quality = LowQ, origin = "ExDog22" }
    , { gmbcID = "GMBC10.178_442", quality = LowQ, origin = "SAMEA2582028" }
    , { gmbcID = "GMBC10.178_845", quality = LowQ, origin = "SAMEA3708621" }
    , { gmbcID = "GMBC10.179_074", quality = LowQ, origin = "SAMN02676138" }
    , { gmbcID = "GMBC10.179_617", quality = LowQ, origin = "SAMN02675505" }
    , { gmbcID = "GMBC10.179_619", quality = LowQ, origin = "SAMN02675503" }
    , { gmbcID = "GMBC10.179_690", quality = LowQ, origin = "SAMN01774283" }
    , { gmbcID = "GMBC10.179_691", quality = LowQ, origin = "SAMN01774024" }
    , { gmbcID = "GMBC10.179_777", quality = LowQ, origin = "SAMEA3708891" }
    , { gmbcID = "GMBC10.179_835", quality = LowQ, origin = "SAMEA3708681" }
    , { gmbcID = "GMBC10.180_224", quality = LowQ, origin = "SAMEA2737879" }
    , { gmbcID = "GMBC10.180_240", quality = LowQ, origin = "SAMEA2737813" }
    , { gmbcID = "GMBC10.180_426", quality = LowQ, origin = "SAMEA2338663" }
    , { gmbcID = "GMBC10.180_558", quality = LowQ, origin = "ExDog45" }
    , { gmbcID = "GMBC10.180_638", quality = LowQ, origin = "SAMEA4378270" }
    , { gmbcID = "GMBC10.180_756", quality = LowQ, origin = "SAMEA2737880" }
    , { gmbcID = "GMBC10.180_968", quality = LowQ, origin = "SAMEA3449330" }
    , { gmbcID = "GMBC10.181_131", quality = LowQ, origin = "SAMEA3708544" }
    , { gmbcID = "GMBC10.181_257", quality = LowQ, origin = "SAMN05545077" }
    , { gmbcID = "GMBC10.181_329", quality = LowQ, origin = "SAMN00691395" }
    , { gmbcID = "GMBC10.181_331", quality = LowQ, origin = "SAMN00001766" }
    , { gmbcID = "GMBC10.181_640", quality = LowQ, origin = "SAMEA2466962" }
    , { gmbcID = "GMBC10.181_712", quality = LowQ, origin = "SAMN01795880" }
    , { gmbcID = "GMBC10.182_372", quality = LowQ, origin = "SAMEA2467002" }
    , { gmbcID = "GMBC10.182_826", quality = LowQ, origin = "SAMN01779907" }
    , { gmbcID = "GMBC10.183_443", quality = LowQ, origin = "SAMN05827159" }
    , { gmbcID = "GMBC10.183_563", quality = LowQ, origin = "SAMEA3664835" }
    , { gmbcID = "GMBC10.183_654", quality = LowQ, origin = "SAMEA3708557" }
    , { gmbcID = "GMBC10.183_764", quality = LowQ, origin = "SRX1670025" }
    , { gmbcID = "GMBC10.184_428", quality = LowQ, origin = "SAMEA3708572" }
    , { gmbcID = "GMBC10.184_952", quality = LowQ, origin = "SAMEA2579985" }
    , { gmbcID = "GMBC10.185_505", quality = LowQ, origin = "SAMEA2582106" }
    , { gmbcID = "GMBC10.185_540", quality = LowQ, origin = "SAMEA3663257" }
    , { gmbcID = "GMBC10.186_522", quality = LowQ, origin = "SAMEA2737709" }
    , { gmbcID = "GMBC10.187_064", quality = LowQ, origin = "SAMEA3449270" }
    , { gmbcID = "GMBC10.187_307", quality = LowQ, origin = "SAMN00767973" }
    , { gmbcID = "GMBC10.187_962", quality = LowQ, origin = "SAMEA1906458" }
    , { gmbcID = "GMBC10.188_462", quality = LowQ, origin = "SAMN04261333" }
    , { gmbcID = "GMBC10.188_501", quality = LowQ, origin = "SAMN03025564" }
    , { gmbcID = "GMBC10.188_610", quality = LowQ, origin = "SAMN01795855" }
    , { gmbcID = "GMBC10.188_614", quality = LowQ, origin = "SAMN01780047" }
    , { gmbcID = "GMBC10.188_615", quality = LowQ, origin = "SAMN01774024" }
    , { gmbcID = "GMBC10.188_618", quality = LowQ, origin = "SAMN00691167" }
    , { gmbcID = "GMBC10.188_634", quality = LowQ, origin = "SAMEA3951793" }
    , { gmbcID = "GMBC10.188_742", quality = LowQ, origin = "SAMEA3708640" }
    , { gmbcID = "GMBC10.188_948", quality = LowQ, origin = "SAMEA3449427" }
    , { gmbcID = "GMBC10.189_029", quality = LowQ, origin = "SAMEA3387456" }
    , { gmbcID = "GMBC10.189_284", quality = LowQ, origin = "SAMEA2467048" }
    , { gmbcID = "GMBC10.189_307", quality = LowQ, origin = "SAMEA2466932" }
    , { gmbcID = "GMBC10.189_533", quality = LowQ, origin = "ExDog24" }
    , { gmbcID = "GMBC10.189_897", quality = LowQ, origin = "SAMN01774111" }
    , { gmbcID = "GMBC10.190_409", quality = LowQ, origin = "SAMEA3663258" }
    , { gmbcID = "GMBC10.191_125", quality = LowQ, origin = "SAMEA2581876" }
    , { gmbcID = "GMBC10.191_459", quality = LowQ, origin = "SRS016585" }
    , { gmbcID = "GMBC10.191_511", quality = LowQ, origin = "SAMN05545060" }
    , { gmbcID = "GMBC10.191_527", quality = LowQ, origin = "SAMN05545024" }
    , { gmbcID = "GMBC10.191_538", quality = LowQ, origin = "SAMN05545009" }
    , { gmbcID = "GMBC10.191_549", quality = LowQ, origin = "SAMN04436777" }
    , { gmbcID = "GMBC10.191_550", quality = LowQ, origin = "SAMN04436774" }
    , { gmbcID = "GMBC10.191_551", quality = LowQ, origin = "SAMN04436770" }
    , { gmbcID = "GMBC10.191_552", quality = LowQ, origin = "SAMN04436732" }
    , { gmbcID = "GMBC10.191_565", quality = LowQ, origin = "SAMN03401296" }
    , { gmbcID = "GMBC10.191_569", quality = LowQ, origin = "SAMN03398940" }
    , { gmbcID = "GMBC10.191_570", quality = LowQ, origin = "SAMN03398927" }
    , { gmbcID = "GMBC10.191_573", quality = LowQ, origin = "SAMN03398796" }
    , { gmbcID = "GMBC10.191_607", quality = LowQ, origin = "SAMN02676217" }
    , { gmbcID = "GMBC10.191_622", quality = LowQ, origin = "SAMN02676168" }
    , { gmbcID = "GMBC10.191_623", quality = LowQ, origin = "SAMN02676167" }
    , { gmbcID = "GMBC10.191_624", quality = LowQ, origin = "SAMN02676166" }
    , { gmbcID = "GMBC10.191_641", quality = LowQ, origin = "SAMN02676150" }
    , { gmbcID = "GMBC10.191_643", quality = LowQ, origin = "SAMN02676149" }
    , { gmbcID = "GMBC10.191_645", quality = LowQ, origin = "SAMN02676142" }
    , { gmbcID = "GMBC10.191_647", quality = LowQ, origin = "SAMN02676139" }
    , { gmbcID = "GMBC10.191_648", quality = LowQ, origin = "SAMN02676137" }
    , { gmbcID = "GMBC10.191_670", quality = LowQ, origin = "SAMN02675497" }
    , { gmbcID = "GMBC10.191_671", quality = LowQ, origin = "SAMN02404522" }
    , { gmbcID = "GMBC10.191_683", quality = LowQ, origin = "SAMN02334115" }
    , { gmbcID = "GMBC10.191_701", quality = LowQ, origin = "SAMN02334054" }
    , { gmbcID = "GMBC10.191_722", quality = LowQ, origin = "SAMN01906337" }
    , { gmbcID = "GMBC10.191_729", quality = LowQ, origin = "SAMN01795962" }
    , { gmbcID = "GMBC10.191_730", quality = LowQ, origin = "SAMN01795937" }
    , { gmbcID = "GMBC10.191_733", quality = LowQ, origin = "SAMN01795864" }
    , { gmbcID = "GMBC10.191_735", quality = LowQ, origin = "SAMN01795812" }
    , { gmbcID = "GMBC10.191_736", quality = LowQ, origin = "SAMN01795803" }
    , { gmbcID = "GMBC10.191_740", quality = LowQ, origin = "SAMN01795781" }
    , { gmbcID = "GMBC10.191_741", quality = LowQ, origin = "SAMN01780298" }
    , { gmbcID = "GMBC10.191_742", quality = LowQ, origin = "SAMN01780057" }
    , { gmbcID = "GMBC10.191_743", quality = LowQ, origin = "SAMN01780047" }
    , { gmbcID = "GMBC10.191_745", quality = LowQ, origin = "SAMN01780004" }
    , { gmbcID = "GMBC10.191_747", quality = LowQ, origin = "SAMN01779982" }
    , { gmbcID = "GMBC10.191_749", quality = LowQ, origin = "SAMN01779927" }
    , { gmbcID = "GMBC10.191_753", quality = LowQ, origin = "SAMN01774317" }
    , { gmbcID = "GMBC10.191_756", quality = LowQ, origin = "SAMN01774024" }
    , { gmbcID = "GMBC10.191_759", quality = LowQ, origin = "SAMN01773416" }
    , { gmbcID = "GMBC10.191_762", quality = LowQ, origin = "SAMN01773395" }
    , { gmbcID = "GMBC10.191_764", quality = LowQ, origin = "SAMN01773349" }
    , { gmbcID = "GMBC10.191_766", quality = LowQ, origin = "SAMN01773313" }
    , { gmbcID = "GMBC10.191_769", quality = LowQ, origin = "SAMN00998693" }
    , { gmbcID = "GMBC10.191_773", quality = LowQ, origin = "SAMN00779899" }
    , { gmbcID = "GMBC10.191_774", quality = LowQ, origin = "SAMN00779630" }
    , { gmbcID = "GMBC10.191_775", quality = LowQ, origin = "SAMN00699746" }
    , { gmbcID = "GMBC10.191_778", quality = LowQ, origin = "SAMN00691414" }
    , { gmbcID = "GMBC10.191_786", quality = LowQ, origin = "SAMEA4378319" }
    , { gmbcID = "GMBC10.191_788", quality = LowQ, origin = "SAMEA4378296" }
    , { gmbcID = "GMBC10.191_793", quality = LowQ, origin = "SAMEA4378271" }
    , { gmbcID = "GMBC10.191_816", quality = LowQ, origin = "SAMEA3951746" }
    , { gmbcID = "GMBC10.191_820", quality = LowQ, origin = "SAMEA3951720" }
    , { gmbcID = "GMBC10.191_829", quality = LowQ, origin = "SAMEA3951685" }
    , { gmbcID = "GMBC10.191_851", quality = LowQ, origin = "SAMEA3879575" }
    , { gmbcID = "GMBC10.191_862", quality = LowQ, origin = "SAMEA3708896" }
    , { gmbcID = "GMBC10.191_864", quality = LowQ, origin = "SAMEA3708884" }
    , { gmbcID = "GMBC10.191_865", quality = LowQ, origin = "SAMEA3708883" }
    , { gmbcID = "GMBC10.191_880", quality = LowQ, origin = "SAMEA3708830" }
    , { gmbcID = "GMBC10.191_890", quality = LowQ, origin = "SAMEA3708773" }
    , { gmbcID = "GMBC10.191_902", quality = LowQ, origin = "SAMEA3708720" }
    , { gmbcID = "GMBC10.191_904", quality = LowQ, origin = "SAMEA3708716" }
    , { gmbcID = "GMBC10.191_905", quality = LowQ, origin = "SAMEA3708714" }
    , { gmbcID = "GMBC10.191_921", quality = LowQ, origin = "SAMEA3708640" }
    , { gmbcID = "GMBC10.191_937", quality = LowQ, origin = "SAMEA3708509" }
    , { gmbcID = "GMBC10.191_941", quality = LowQ, origin = "SAMEA3708483" }
    , { gmbcID = "GMBC10.191_961", quality = LowQ, origin = "SAMEA3664969" }
    , { gmbcID = "GMBC10.192_057", quality = LowQ, origin = "SAMEA3449427" }
    , { gmbcID = "GMBC10.192_061", quality = LowQ, origin = "SAMEA3449376" }
    , { gmbcID = "GMBC10.192_067", quality = LowQ, origin = "SAMEA3449323" }
    , { gmbcID = "GMBC10.192_068", quality = LowQ, origin = "SAMEA3449310" }
    , { gmbcID = "GMBC10.192_069", quality = LowQ, origin = "SAMEA3449308" }
    , { gmbcID = "GMBC10.192_071", quality = LowQ, origin = "SAMEA3449305" }
    , { gmbcID = "GMBC10.192_072", quality = LowQ, origin = "SAMEA3449299" }
    , { gmbcID = "GMBC10.192_073", quality = LowQ, origin = "SAMEA3449291" }
    , { gmbcID = "GMBC10.192_080", quality = LowQ, origin = "SAMEA3449242" }
    , { gmbcID = "GMBC10.192_081", quality = LowQ, origin = "SAMEA3449240" }
    , { gmbcID = "GMBC10.192_112", quality = LowQ, origin = "SAMEA3387435" }
    , { gmbcID = "GMBC10.192_135", quality = LowQ, origin = "SAMEA3136653" }
    , { gmbcID = "GMBC10.192_136", quality = LowQ, origin = "SAMEA3136652" }
    , { gmbcID = "GMBC10.192_215", quality = LowQ, origin = "SAMEA2737878" }
    , { gmbcID = "GMBC10.192_221", quality = LowQ, origin = "SAMEA2737852" }
    , { gmbcID = "GMBC10.192_222", quality = LowQ, origin = "SAMEA2737847" }
    , { gmbcID = "GMBC10.192_234", quality = LowQ, origin = "SAMEA2737739" }
    , { gmbcID = "GMBC10.192_301", quality = LowQ, origin = "SAMEA2580271" }
    , { gmbcID = "GMBC10.192_321", quality = LowQ, origin = "SAMEA2467016" }
    , { gmbcID = "GMBC10.192_349", quality = LowQ, origin = "SAMEA2338855" }
    , { gmbcID = "GMBC10.192_350", quality = LowQ, origin = "SAMEA2338854" }
    , { gmbcID = "GMBC10.192_353", quality = LowQ, origin = "SAMEA2338845" }
    , { gmbcID = "GMBC10.192_362", quality = LowQ, origin = "SAMEA2338769" }
    , { gmbcID = "GMBC10.192_471", quality = LowQ, origin = "ExDog44" }
    , { gmbcID = "GMBC10.192_475", quality = LowQ, origin = "ExDog37" }
    , { gmbcID = "GMBC10.192_478", quality = LowQ, origin = "ExDog34" }
    , { gmbcID = "GMBC10.193_791", quality = LowQ, origin = "SAMN04436770" }
    , { gmbcID = "GMBC10.193_963", quality = LowQ, origin = "SAMN01985131" }
    , { gmbcID = "GMBC10.193_979", quality = LowQ, origin = "SAMN00699742" }
    , { gmbcID = "GMBC10.194_089", quality = LowQ, origin = "SAMEA3708778" }
    , { gmbcID = "GMBC10.194_103", quality = LowQ, origin = "SAMEA3708694" }
    , { gmbcID = "GMBC10.194_615", quality = LowQ, origin = "SAMEA2581898" }
    , { gmbcID = "GMBC10.195_275", quality = LowQ, origin = "SAMN01780298" }
    , { gmbcID = "GMBC10.195_405", quality = LowQ, origin = "SAMN04436810" }
    , { gmbcID = "GMBC10.195_846", quality = LowQ, origin = "SAMEA2737748" }
    , { gmbcID = "GMBC10.195_894", quality = LowQ, origin = "SAMN05827126" }
    , { gmbcID = "GMBC10.196_361", quality = LowQ, origin = "SAMN01795880" }
    , { gmbcID = "GMBC10.199_081", quality = LowQ, origin = "SAMN03399021" }
    , { gmbcID = "GMBC10.201_066", quality = LowQ, origin = "SAMEA3951694" }
    , { gmbcID = "GMBC10.201_359", quality = LowQ, origin = "SAMN02676151" }
    , { gmbcID = "GMBC10.201_953", quality = LowQ, origin = "SAMN01773338" }
    , { gmbcID = "GMBC10.201_963", quality = LowQ, origin = "SAMN00691395" }
    , { gmbcID = "GMBC10.202_278", quality = LowQ, origin = "SAMEA3449326" }
    , { gmbcID = "GMBC10.203_570", quality = LowQ, origin = "SAMN05545038" }
    , { gmbcID = "GMBC10.203_580", quality = LowQ, origin = "SAMN05545028" }
    , { gmbcID = "GMBC10.203_831", quality = LowQ, origin = "SAMN02675499" }
    , { gmbcID = "GMBC10.204_030", quality = LowQ, origin = "SAMN01779912" }
    , { gmbcID = "GMBC10.204_037", quality = LowQ, origin = "SAMN01773310" }
    , { gmbcID = "GMBC10.204_048", quality = LowQ, origin = "SAMN00690842" }
    , { gmbcID = "GMBC10.204_433", quality = LowQ, origin = "SAMEA3708501" }
    , { gmbcID = "GMBC10.205_238", quality = LowQ, origin = "SAMEA2737767" }
    , { gmbcID = "GMBC10.205_427", quality = LowQ, origin = "SAMEA2579963" }
    , { gmbcID = "GMBC10.205_878", quality = LowQ, origin = "ExDog14" }
    , { gmbcID = "GMBC10.207_178", quality = LowQ, origin = "SAMN04261359" }
    , { gmbcID = "GMBC10.208_296", quality = LowQ, origin = "SAMN04261332" }
    , { gmbcID = "GMBC10.208_346", quality = LowQ, origin = "SAMN02676150" }
    , { gmbcID = "GMBC10.208_746", quality = LowQ, origin = "SAMEA3449339" }
    , { gmbcID = "GMBC10.208_768", quality = LowQ, origin = "SAMEA3449209" }
    , { gmbcID = "GMBC10.209_289", quality = LowQ, origin = "SAMN02676139" }
    , { gmbcID = "GMBC10.209_348", quality = LowQ, origin = "SAMN02676152" }
    , { gmbcID = "GMBC10.209_547", quality = LowQ, origin = "SAMEA4378270" }
    , { gmbcID = "GMBC10.209_642", quality = LowQ, origin = "SAMN03401296" }
    , { gmbcID = "GMBC10.209_740", quality = LowQ, origin = "SAMEA2338794" }
    , { gmbcID = "GMBC10.210_083", quality = LowQ, origin = "SAMEA3951790" }
    , { gmbcID = "GMBC10.211_391", quality = LowQ, origin = "SAMEA2467045" }
    , { gmbcID = "GMBC10.212_454", quality = LowQ, origin = "SAMN01795812" }
    , { gmbcID = "GMBC10.212_497", quality = LowQ, origin = "SAMEA2580019" }
    , { gmbcID = "GMBC10.212_681", quality = LowQ, origin = "SAMN02334111" }
    , { gmbcID = "GMBC10.214_404", quality = LowQ, origin = "SAMN05827174" }
    , { gmbcID = "GMBC10.215_466", quality = LowQ, origin = "SAMN02334084" }
    , { gmbcID = "GMBC10.215_771", quality = LowQ, origin = "SAMN01773310" }
    , { gmbcID = "GMBC10.215_889", quality = LowQ, origin = "SAMEA4378273" }
    , { gmbcID = "GMBC10.215_928", quality = LowQ, origin = "SAMEA4378248" }
    , { gmbcID = "GMBC10.219_447", quality = LowQ, origin = "SAMEA2580302" }
    , { gmbcID = "GMBC10.219_590", quality = LowQ, origin = "SAMEA2579912" }
    , { gmbcID = "GMBC10.222_255", quality = LowQ, origin = "SAMN01889407" }
    , { gmbcID = "GMBC10.222_260", quality = LowQ, origin = "SAMN01795792" }
    , { gmbcID = "GMBC10.222_264", quality = LowQ, origin = "SAMN01773492" }
    , { gmbcID = "GMBC10.222_872", quality = LowQ, origin = "SAMN04436895" }
    , { gmbcID = "GMBC10.222_992", quality = LowQ, origin = "SAMEA3879515" }
    , { gmbcID = "GMBC10.223_473", quality = LowQ, origin = "SAMN00699766" }
    , { gmbcID = "GMBC10.223_729", quality = LowQ, origin = "SAMN01780039" }
    , { gmbcID = "GMBC10.223_999", quality = LowQ, origin = "SAMEA4378257" }
    , { gmbcID = "GMBC10.224_129", quality = LowQ, origin = "SAMEA4378258" }
    , { gmbcID = "GMBC10.224_316", quality = LowQ, origin = "SAMN01773395" }
    , { gmbcID = "GMBC10.226_111", quality = LowQ, origin = "SAMN05826913" }
    , { gmbcID = "GMBC10.233_967", quality = LowQ, origin = "SAMN05545028" }
    , { gmbcID = "GMBC10.235_738", quality = LowQ, origin = "SAMN02676167" }
    , { gmbcID = "GMBC10.235_807", quality = LowQ, origin = "SAMN02676153" }
    , { gmbcID = "GMBC10.235_819", quality = LowQ, origin = "SAMN02676151" }
    , { gmbcID = "GMBC10.235_839", quality = LowQ, origin = "SAMN02676147" }
    , { gmbcID = "GMBC10.235_890", quality = LowQ, origin = "SAMN02676139" }
    , { gmbcID = "GMBC10.235_896", quality = LowQ, origin = "SAMN02676136" }
    , { gmbcID = "GMBC10.236_197", quality = LowQ, origin = "SAMN02675505" }
    , { gmbcID = "GMBC10.240_142", quality = LowQ, origin = "SAMEA3708884" }
    , { gmbcID = "GMBC10.240_845", quality = LowQ, origin = "SAMEA3708773" }
    , { gmbcID = "GMBC10.241_167", quality = LowQ, origin = "SAMEA3708720" }
    , { gmbcID = "GMBC10.255_588", quality = LowQ, origin = "SAMEA2467013" }
    , { gmbcID = "GMBC10.260_145", quality = LowQ, origin = "ExDog44" }
    , { gmbcID = "GMBC10.262_298", quality = LowQ, origin = "SAMEA2737695" }
    , { gmbcID = "GMBC10.263_466", quality = LowQ, origin = "SAMN03270026" }
    , { gmbcID = "GMBC10.263_640", quality = LowQ, origin = "SAMN00998693" }
    , { gmbcID = "GMBC10.263_816", quality = LowQ, origin = "SAMEA3708658" }
    , { gmbcID = "GMBC10.263_820", quality = LowQ, origin = "SAMEA1906531" }
    , { gmbcID = "GMBC10.264_920", quality = LowQ, origin = "SAMN04436709" }
    , { gmbcID = "GMBC10.265_491", quality = LowQ, origin = "SAMN00991546" }
    , { gmbcID = "GMBC10.265_749", quality = LowQ, origin = "SAMN05826567" }
    , { gmbcID = "GMBC10.266_019", quality = LowQ, origin = "SAMN00690843" }
    , { gmbcID = "GMBC10.266_288", quality = LowQ, origin = "SAMN05827148" }
    , { gmbcID = "GMBC10.269_375", quality = LowQ, origin = "SAMN03270385" }
    , { gmbcID = "GMBC10.270_241", quality = LowQ, origin = "SAMEA3449304" }
    , { gmbcID = "GMBC10.270_286", quality = LowQ, origin = "SAMN02334083" }
    ]

