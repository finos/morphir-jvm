module Arithmetic.StringLiteralMatch exposing (stringLiteralMatch)

stringLiteralMatch : String -> Int
stringLiteralMatch value =
    case value of
        "vip" ->
            10

        _ ->
            0
