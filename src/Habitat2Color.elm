module Habitat2Color exposing (habitat2color)

import Dict as Dict

habitat2color : String -> String
habitat2color b = Dict.get b habitat2colorTable |> Maybe.withDefault "#666666"

habitat2colorTable : Dict.Dict String String
habitat2colorTable = Dict.fromList [
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
