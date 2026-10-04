module Arithmetic.TupleDestructureAdd exposing (tupleDestructureAdd)

tupleDestructureAdd : ( Int, Int ) -> Int
tupleDestructureAdd pair =
    let
        ( x, y ) =
            pair
    in
    x + y
