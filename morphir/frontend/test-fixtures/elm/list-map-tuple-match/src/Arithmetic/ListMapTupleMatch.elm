module Arithmetic.ListMapTupleMatch exposing (sumPairs)

sumPairs : List ( Int, Int ) -> List Int
sumPairs values =
    List.map
        (\pair ->
            case pair of
                ( left, right ) ->
                    left + right
        )
        values
