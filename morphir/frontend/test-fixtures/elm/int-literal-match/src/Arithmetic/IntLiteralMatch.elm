module Arithmetic.IntLiteralMatch exposing (intLiteralMatch)

intLiteralMatch : Int -> Int
intLiteralMatch value =
    case value of
        0 ->
            100

        1 ->
            200

        _ ->
            value + 1
