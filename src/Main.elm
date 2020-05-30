module Main exposing (..)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Popover as Popover
import Bootstrap.Text as Text
import Bootstrap.Table as Table
import Bootstrap.Spinner as Spinner
import Bytes
import Bytes.Encode as E

import Html exposing (..)
import Html.Attributes exposing (class, for, href, placeholder)
import Html.Attributes as HAttr
import Html.Events exposing (..)
 
import Http
import Process as Process
import Task as Task

import File.Download as Download

import Json.Decode as D
import Browser
import Browser.Navigation as Nav

import Enog as ENOG
import GenomeBins as GB

type InitModel = InitModel
type ModeChoice =
        Information
        | HaveSequence
        | HaveENOG
        | HaveGenome
        | WantBin
        | WantDownload 

type alias SequenceQuery = 
    { facontent : String }

type Model =
        ModelIsInitial
        | ModelIsLoading
        | ModelIsGenome
        | ModelIsSequenceQuery SequenceQuery
        | ModelIsENOGQuery String
        | ModelIsBinQuery String
        | ModelIsDownload

type Msg
    = NoMsg
    | SelectMode ModeChoice
    | UpdateFacontent String
    | UpdateQ String
    | SetExample
    | SubmitData
    | GotoResults


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

init : () -> ( Model, Cmd Msg )
init () =
    ( ModelIsSequenceQuery { facontent = "" }
    , Cmd.none
    )

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
    NoMsg -> ( model, Cmd.none )
    SubmitData -> case model of
            ModelIsSequenceQuery qmodel -> ( ModelIsLoading , submitData qmodel )
            _ -> ( model, Cmd.none )
    UpdateFacontent fa -> ( ModelIsSequenceQuery { facontent = fa }, Cmd.none )
    UpdateQ q -> case model of
        ModelIsENOGQuery _ -> ( ModelIsENOGQuery q, Cmd.none )
        _ -> ( ModelIsBinQuery q, Cmd.none )
    SetExample -> case model of
        ModelIsSequenceQuery _ -> ( ModelIsSequenceQuery { facontent = proteinQueryExampleData }, Cmd.none )
        ModelIsENOGQuery _ -> ( ModelIsENOGQuery enogExampleData, Cmd.none )
        _  -> ( ModelIsBinQuery binQExampleData, Cmd.none )
    SelectMode mode -> case mode of
        Information -> ( model, Cmd.none )
        HaveSequence -> ( ModelIsSequenceQuery { facontent = "" }, Cmd.none )
        HaveENOG -> ( ModelIsENOGQuery "", Cmd.none )
        HaveGenome -> ( ModelIsGenome, Cmd.none )
        WantBin -> ( ModelIsBinQuery "", Cmd.none )
        WantDownload -> ( ModelIsDownload, Cmd.none )
    GotoResults -> ( model, Nav.load "SearchResults.html" )



--submitData : QueryModel -> Cmd Msg
submitData _ = Task.perform (\() -> GotoResults) (Process.sleep 8000 |> Task.andThen (\_ -> Task.succeed ()))
    {- Http.post
        { url = "http://gmgc.embl.de/downloads/v1.0/query/sequence"
        , body = Http.multipartBody
                    [ Http.bytesPart "text/x-fasta" "fasta" (E.encode <| E.string model.facontent)
                    ]
        , expect = Http.expectJson ResultsData0 decodeSearchResults
        } -}

activeMode : Model -> ModeChoice
activeMode m = case m of
    ModelIsInitial -> Information
    ModelIsLoading -> HaveSequence
    ModelIsENOGQuery _ -> HaveENOG
    ModelIsGenome -> HaveGenome
    ModelIsBinQuery _ -> WantBin
    ModelIsSequenceQuery _ -> HaveSequence
    ModelIsDownload -> WantDownload



view : Model -> Browser.Document Msg
view model =
    { title = "Global Microbial Gene Catalog"
    , body =
        [ CDN.stylesheet
        , CDN.fontAwesome
        , Grid.container []
            [ Grid.simpleRow
                [ Grid.col []
                    [ header
                    , Html.hr [] []
                    , intro
                    , Html.hr [] []
                    , viewModel model
                    , Html.hr [] []
                    , outro
                    , Html.hr [] []
                    , footer
                    ]
                ]
            ]
        ]
    }


header : Html Msg
header =
    Grid.simpleRow
        [ Grid.col [] [ Html.h4 [] [ Html.text "GMGC" ] ]
        , Grid.col [] [ Html.a [ href "http://gmgc.embl.de/help.cgi" ] [ Html.h4 [] [ Html.text "Docs" ] ] ]
        , Grid.col [] [ Html.a [ href "http://gmgc.embl.de/download.cgi" ] [ Html.h4 [] [ Html.text "Download" ] ] ]
        , Grid.col [] [ Html.a [ href "http://gmgc.embl.de/about.cgi" ] [ Html.h4 [] [ Html.text "About & Contact" ] ] ]
        ]


intro : Html Msg
intro =
    Html.div []
        [Html.h1 [] [Html.text "Global Microbial Gene Catalog"]
        ,Html.p  []
            [ Html.text "The "
            , Html.strong [] [Html.text "Global Microbial Gene Catalog ("
                             , Html.em [] [Html.text "GMGC"]
                             , Html.text ")"]
            , Html.text " is an integrated, consistently-processed, gene catalog of the microbial world, combining metagenomics and high-quality sequenced isolates."
            ]
        ]


outro : Html Msg
outro =
    Html.div [] [ ]


footer : Html Msg
footer =
    Html.text "Copyright 2018-2020 GMGC Authors"

viewModel : Model -> Html Msg
viewModel model = Grid.simpleRow
        [ Grid.col []
            [ viewChoice model
            , Html.hr [] []
            , case model of
                ModelIsInitial -> viewInitial
                ModelIsLoading -> Html.div []
                    [Html.div []
                        [Spinner.spinner [ Spinner.color Text.primary, Spinner.grow ] [ ]
                        ,Html.p [] [ Html.text "Waiting for results..." ]
                        ,Html.p [] [ Html.text "Normally, it should not take more than 10-20 seconds, but if the server load is high, it can take up to a minute or two." ]
                        ]
                    ]
                ModelIsSequenceQuery qmodel -> viewSequenceQuery qmodel
                ModelIsENOGQuery q -> viewEnogQ q
                ModelIsBinQuery q -> viewBinQ q
                ModelIsGenome-> viewGenome
                ModelIsDownload -> viewDownload
            ]
        ]

viewChoice model =
    let
        buttonStyle who =
            if who == activeMode model then
                [ Button.info, Button.onClick (SelectMode who) ]

            else
                [ Button.outlineSecondary, Button.onClick (SelectMode who) ]
    in
    Grid.simpleRow
        [ Grid.col [] <|
            [ Html.h2 [] [ Html.text "GMGC Tools" ]
            , Grid.simpleRow
                [ Grid.col [] [ Button.button (buttonStyle HaveSequence) [ Html.text "Find homologues to a sequence (BLAST-like)" ] ]
                , Grid.col [] [ Button.button (buttonStyle HaveENOG) [ Html.text "Find a gene family (eggNOG orthologs)" ] ]
                , Grid.col [] [ Button.button (buttonStyle HaveGenome) [ Html.text "Map a (meta)genome to the GMGC"  ] ]
                , Grid.col [] [ Button.button (buttonStyle WantBin) [ Html.text "Look for genomic bins"  ] ]
                ]
            ]
        ]
viewInitial= Html.div [] [Html.p [] [Html.text "START"] ]
viewResults r = Html.div [] [Html.p [] [Html.text "RESULTS"] ]
viewDownload = Html.div [] [Html.p [] [Html.text "Download much?"] ]

viewSequenceQuery qmodel = 
    let
        faerror = validateFasta qmodel.facontent
    in
        Grid.simpleRow
            [ Grid.col [] <|
                [ Html.h3 [] [Html.text "Query by sequence (BLAST-like)"]
                , case faerror of
                    Nothing ->
                        Html.text ""

                    Just err ->
                        Alert.simpleWarning [] [ err ]
                , Form.group []
                    [ Html.label [ for "fasta" ]
                        [ Html.text " Input amino acid FASTA format"
                        ]
                    , Textarea.textarea <|
                        [ Textarea.id "fasta"
                        , Textarea.rows 10
                        , Textarea.onInput UpdateFacontent
                        , Textarea.attrs [ placeholder placeholderText ]
                        , Textarea.value qmodel.facontent
                        ]
                            ++ (case faerror of
                                    Nothing ->
                                        []

                                    Just _ ->
                                        [ Textarea.danger ]
                               )
                    , Grid.row [ Row.rightXl ]
                        [ Grid.col [] [ Html.text "" ]
                        , Grid.col [ Col.textAlign Text.alignXsRight ]
                            [ Button.button [ Button.small, Button.outlineSecondary, Button.onClick SetExample ] [ Html.text "Example" ] ]
                        ]
                    , Button.button [ Button.primary, Button.onClick SubmitData ] [ Html.text "Submit" ]
                    ] ] ]
     
activeNogs q =
    if String.isEmpty q
        then ENOG.bactnogs45
        else List.filter (\b -> String.contains q b.description || String.contains q b.enogID) ENOG.bactnogs45

viewEnogQ q =
    let
        active = activeNogs q
    in Grid.simpleRow [Grid.col []
        [ Html.h4 [] [ Html.text "Pick an eggNOG orthologous group below" ]
        , Grid.simpleRow
            [Grid.col []
                [ Html.label [ for "eq" ] [ Html.strong [] [ Html.text "Filter by: " ] ]
                , Html.input [ HAttr.placeholder "query", HAttr.value q, onInput UpdateQ ] [] ]
            ,Grid.col [] [ Button.button [ Button.small, Button.outlineSecondary, Button.onClick SetExample ] [ Html.text "Example" ]
                ]
            ]
        , Html.p [] [Html.text <| (String.fromInt <| List.length active) ++ " orthologous groups matching."]
        , Table.table
            { options = [ Table.striped, Table.hover ]
            , thead =  Table.simpleThead
                [ Table.th [] [ Html.text "Eggnog OG name" ]
                , Table.th [] [ Html.text "Description" ]
                ]
            , tbody = Table.tbody []
                    (List.map (\e ->
                        Table.tr []
                            [ Table.td [] [ Html.a [href ("http://gmgc.embl.de/search.cgi?search_id="++e.enogID)]
                                                    [Html.text e.enogID ]]
                            , Table.td [] [ Html.text e.description ]
                            ]) (List.take 1000 active))
            }
        , (if List.length active > 1000
            then Html.p [] [Html.text "... (only 1,000 groups shown)" ]
            else Html.p [] [])
        ] ]

activeBinsQ : String -> List GB.GenomeBinGroup
activeBinsQ q =
    if String.isEmpty q
        then GB.genomebins
        else List.filter (\b -> String.contains q b.taxID) GB.genomebins

viewBinQ q =
    let
        active = activeBinsQ q
    in Grid.simpleRow [Grid.col []
        [ Html.h3 [] [ Html.text "Global Microbial Genomic Bins collection version 1.0 (GMBC 1.0)" ]
        , Html.p [] [ Html.text "The GMBCv1.0 include >250k genomic bins, grouped into three quality classes: "
                    , Html.em [] [Html.text "high-quality"]
                    , Html.text ", "
                    , Html.em [] [Html.text "medium-quality"]
                    , Html.text ", and "
                    , Html.em [] [Html.text "low-quality"]
                    , Html.text ". High and medium quality bins can be considered MAGs (metagenome-assembled genomes)."
                    ]
        , Html.h4 [] [ Html.text "You can filter the genomic bins by taxonomic assignment below" ]
        , Grid.simpleRow
            [Grid.col []
                [ Html.label [ for "eq" ] [ Html.strong [] [ Html.text "Filter by: " ] ]
                , Html.input [ HAttr.placeholder "query", HAttr.value q, onInput UpdateQ ] [] ]
            ,Grid.col [] [ Button.button [ Button.small, Button.outlineSecondary, Button.onClick SetExample ] [ Html.text "Example" ]
                ]
            ]
        , Html.p [] [Html.text <| (String.fromInt <| List.length active) ++ " matches."]
        , Table.table
            { options = [ Table.striped, Table.hover ]
            , thead =  Table.simpleThead
                [ Table.th [] [ Html.text "Taxonomic prediction (GTDB)" ]
                , Table.th [] [ Html.text "# high-quality bins" ]
                , Table.th [] [ Html.text "# medium-quality bins" ]
                , Table.th [] [ Html.text "# low-quality bins" ]
                ]
            , tbody = Table.tbody []
                    (List.map (\bg ->
                        Table.tr []
                            [ Table.td [] [ Html.a [href ("GenomeBinSearchResult.html")]
                                                    [Html.text bg.taxID ]]
                            , Table.td [] [ Html.text <| String.fromInt bg.highQ ]
                            , Table.td [] [ Html.text <| String.fromInt bg.medQ ]
                            , Table.td [] [ Html.text <| String.fromInt bg.lowQ ]
                            ]) (List.take 1000 active))
            }
        , (if List.length active > 1000
            then Html.p [] [Html.text "... (only 1,000 groups shown)" ]
            else Html.p [] [])
        ] ]

viewGenome = Html.div []
    [ Html.h3 [] [Html.text "Mapping a prokaryotic genome to the GMGC" ]
    , Html.p [] [Html.text "This will enable you to find metagenomic samples where your genome is present as well as any MAGs that are similar."]
    , Html.p [] [Html.text "This functionality is available with the "
                ,Html.a [ href "http://gmgc.embl.de/help.cgi" ] [ Html.text "Genome2GMGMC" ]
                ,Html.text " tool."
                ]
    , Html.p [] [Html.text "Download it from "
                , Html.a [ href "https://github.com/psj1977/Genome2gmgc" ] [ Html.text "Github" ]
                , Html.text ". There you will also find instructions on how to use it."
                ]
    , Html.h3 [] [Html.text "Mapping a metagenome to the GMGC" ]
    , Html.p [] [Html.text "There are two possible workflows"]
    , Html.ol []
        [ Html.li [] [Html.text "Using the GMGC as a resource for profiling metagenomes"]
        , Html.li [] [Html.text "Assembling and finding novel data"] ]
    , Html.h4 [] [Html.text "Profiling a metagenome to the GMGC" ]
    , Html.p [] [Html.text "Use "
                , Html.a [ href "https://ngless.embl.de/" ] [ Html.text "NGLess" ]
                , Html.text " and the "
                , Html.code [] [Html.text "gmgc"]
                ,Html.text " module."]
    , Html.h4 [] [Html.text "Finding the novelty in your metagenome" ]
    , Html.p [] [Html.text "Use "
                , Html.a [ href "https://ngless.embl.de/" ] [ Html.text "NGLess" ]
                , Html.text " to assemble and predict genes and then use "
                , Html.a [ href "https://github.com/psj1977/Genome2gmgc" ] [ Html.text "Genome2gmgc" ]
                , Html.text " to assign those genes to the GMGC."
                ]
    ]

validateFasta : String -> Maybe (Html Msg)
validateFasta fa =
        let
            nr_seqs = List.length <| List.filter (\ell -> (String.startsWith ">" ell)) <| String.split "\n" fa
            lines = List.filter (\ell -> not (String.startsWith ">" ell)) <| String.split "\n" fa
            totalLen = List.sum <| List.map String.length lines
            isOnlyNucleotides = List.all (\ell -> String.all isNuc ell) lines
            isNuc c = (c == 'A' || c == 'C' || c == 'T' || c == 'G'
                        || c == 'a' || c == 'c' || c == 'g' || c == 'g'
                        || c == 'n' || c == 'N')
        in
            if nr_seqs > 1
            then Just <| Html.div []
                    [Html.p [] [Html.text "Due to resource constraints, this web interface only supports a single sequence." ]
                    ,Html.p [] [Html.text "You can either:"]
                    ,Html.ol []
                        [Html.li [] [Html.text "Use our API for multiple queries." ]
                        ,Html.li [] [Html.text "Download the database for local use."]
                        ]
                    ]
            else if isOnlyNucleotides && totalLen > 100
                    then Just <| Html.div []
                                    [Html.p [] [Html.text "This sequence look suspiciously like DNA. "]
                                    ,Html.p [] [Html.text "Online sequence query works only for protein sequences." ]]
                    else Nothing
placeholderText = ">ProteinID\nMEPEPAGAD...\n"

proteinQueryExampleData : String
proteinQueryExampleData = """>Query
MEPADACAPCNWNEYVPLPNVPQPGRRPFPTFPGQGPFNPKIKWPQGY
"""
enogExampleData = "Sodium glutamate symporter"


binQExampleData : String
binQExampleData = "Escherichia"
