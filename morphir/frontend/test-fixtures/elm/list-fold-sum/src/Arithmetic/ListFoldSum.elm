module Arithmetic.ListFoldSum exposing (sum)

sum : List Int -> Int
sum values =
    List.foldl (\value acc -> acc + value) 0 values
