module Arithmetic.MatchRed exposing (matchRed)

import Arithmetic.Color exposing (Color(..))

matchRed : Color -> Bool
matchRed color =
    case color of
        Red ->
            True

        Blue ->
            False
