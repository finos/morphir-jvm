module Arithmetic.LocalVal exposing (addOne)

addOne : Int -> Int
addOne value =
    let
        next =
            value + 1
    in
    next
