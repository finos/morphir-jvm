module Arithmetic.BooleanLiteralMatch exposing (booleanLiteralMatch)

booleanLiteralMatch : Bool -> Int
booleanLiteralMatch value =
    case value of
        True ->
            1

        False ->
            0
