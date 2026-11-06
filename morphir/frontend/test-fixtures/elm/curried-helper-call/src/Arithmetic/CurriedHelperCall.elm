module Arithmetic.CurriedHelperCall exposing (addThenAdd, combineCurried)

addThenAdd : Int -> Int -> Int -> Int
addThenAdd a b c =
    a + b + c


combineCurried : Int -> Int -> Int -> Int
combineCurried a b c =
    addThenAdd a b c
