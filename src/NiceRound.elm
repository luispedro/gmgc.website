module NiceRound exposing (niceRound)

niceRound : Float -> String
niceRound x =
    let
        log10 = logBase 10 x
    in if x == 0.0
        then "0.0"
        else if log10 < -3
            then sciNotation x
            else directNotation x

sciNotation : Float -> String
sciNotation x =
    let
        (v,n) = splitSci x
    in directNotation v ++ "e-" ++ String.fromInt n

splitSci x = splitSci1 0 x

splitSci1 n x = if x >= 1.0
                    then (x,n)
                    else splitSci1 (n + 1) (x * 10.0)

directNotation x = String.fromInt (truncate x) ++ "." ++ directNotation1 (x - (toFloat (truncate x)))

directNotation1 x =
    let
        x10 = 10.0 * x
    in if x10 >= 1.0
        then String.fromInt (truncate x10) ++ directNotation2 1 (x10 - (toFloat (truncate x10)))
        else "0" ++ directNotation1 x10

directNotation2 n x =
    if n == 0
        then ""
        else let x10 = 10.0 * x
            in String.fromInt (truncate x10) ++ directNotation2 (n - 1) x10

