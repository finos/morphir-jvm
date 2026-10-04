module Arithmetic.ExtractFirstOf4 exposing (extractFirstOf4)

extractFirstOf4 : ( Int, String, Bool, Float ) -> Int
extractFirstOf4 quadruple =
    case quadruple of
        ( first, _, _, _ ) ->
            first
