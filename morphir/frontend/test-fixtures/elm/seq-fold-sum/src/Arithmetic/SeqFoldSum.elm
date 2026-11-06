module Arithmetic.SeqFoldSum exposing (total)

total : List Int -> Int
total values =
    List.foldl (\value acc -> acc + value) 0 values
