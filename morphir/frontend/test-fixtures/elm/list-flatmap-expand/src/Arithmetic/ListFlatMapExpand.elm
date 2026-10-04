module Arithmetic.ListFlatMapExpand exposing (expand)

expand : List Int -> List Int
expand values =
    List.concatMap (\value -> [ value, value + 1 ]) values
