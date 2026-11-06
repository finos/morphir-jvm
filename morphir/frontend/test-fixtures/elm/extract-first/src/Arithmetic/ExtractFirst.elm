module Arithmetic.ExtractFirst exposing (extractFirst)

extractFirst : ( Int, String ) -> Int
extractFirst pair =
    case pair of
        ( first, _ ) ->
            first
