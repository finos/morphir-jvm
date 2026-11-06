module Arithmetic.LocalValChain exposing (sumAndDouble)

sumAndDouble : Int -> Int -> Int
sumAndDouble a b =
    let
        sum =
            a + b

        doubled =
            sum + sum
    in
    doubled
