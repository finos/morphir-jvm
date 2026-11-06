module Arithmetic.TupleDestructureAdd3 exposing (tupleDestructureAdd3)

tupleDestructureAdd3 : ( Int, Int, Int ) -> Int
tupleDestructureAdd3 triple =
    let
        ( x, y, z ) =
            triple
    in
    x + y + z
