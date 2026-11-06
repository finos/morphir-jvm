module Arithmetic.ExtractFirstOf3 exposing (extractFirstOf3)

extractFirstOf3 : ( Int, String, Bool ) -> Int
extractFirstOf3 triple =
    case triple of
        ( first, _, _ ) ->
            first
