module Arithmetic.ForYieldExpand exposing (expand)

expand : List Int -> List Int
expand values =
    List.concatMap (\value -> List.map (\next -> next) [ value, value + 1 ]) values
