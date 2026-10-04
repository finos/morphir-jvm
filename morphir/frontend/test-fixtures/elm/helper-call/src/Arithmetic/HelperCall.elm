module Arithmetic.HelperCall exposing (addThree, combine)

addThree : Int -> Int -> Int -> Int
addThree a b c =
    a + b + c


combine : Int -> Int -> Int -> Int
combine a b c =
    addThree a b c
