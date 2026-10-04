module Arithmetic.ListMapIncrement exposing (incrementAll)

incrementAll : List Int -> List Int
incrementAll values =
    List.map (\value -> value + 1) values
