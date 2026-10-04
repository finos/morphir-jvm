module Arithmetic.SeqMapIncrement exposing (increment)

increment : List Int -> List Int
increment values =
    List.map (\value -> value + 1) values
