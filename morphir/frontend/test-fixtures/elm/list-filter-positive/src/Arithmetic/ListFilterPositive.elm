module Arithmetic.ListFilterPositive exposing (keepPositive)

keepPositive : List Int -> List Int
keepPositive values =
    List.filter (\value -> value > 0) values
