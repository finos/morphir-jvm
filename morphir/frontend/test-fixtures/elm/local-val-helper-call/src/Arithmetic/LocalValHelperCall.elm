module Arithmetic.LocalValHelperCall exposing (addThree, addOneMore)

addThree : Int -> Int -> Int -> Int
addThree a b c =
    a + b + c


addOneMore : Int -> Int -> Int -> Int
addOneMore a b c =
    let
        total =
            addThree a b c
    in
    total + 1
