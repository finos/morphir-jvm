module Arithmetic.FloatLiteralMatch exposing (floatLiteralMatch)

floatLiteralMatch : Float -> Int
floatLiteralMatch value =
    case value of
        1.5 ->
            10

        2.5 ->
            20

        _ ->
            0
