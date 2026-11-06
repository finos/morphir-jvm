module Arithmetic.TupleDestructureAdd4 exposing (tupleDestructureAdd4)

tupleDestructureAdd4 : ( Int, Int, Int, Int ) -> Int
tupleDestructureAdd4 quadruple =
    let
        ( w, x, y, z ) =
            quadruple
    in
    w + x + y + z
