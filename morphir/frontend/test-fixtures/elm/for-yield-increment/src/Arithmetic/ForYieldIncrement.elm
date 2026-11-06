module Arithmetic.ForYieldIncrement exposing (increment)

increment : List Int -> List Int
increment values =
    List.map (\value -> value + 1) values
