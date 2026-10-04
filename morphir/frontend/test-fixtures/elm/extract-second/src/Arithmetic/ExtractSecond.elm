module Arithmetic.ExtractSecond exposing (extractSecond)

extractSecond : ( Int, String ) -> String
extractSecond pair =
    case pair of
        ( _, second ) ->
            second
