module Arithmetic.GetPrimaryValue exposing (getPrimaryValue)

import Arithmetic.ColorWithThreeValues exposing (ColorWithThreeValues(..))


getPrimaryValue : ColorWithThreeValues -> Int
getPrimaryValue color =
    case color of
        Named _ intensity _ ->
            intensity

        Custom red green blue ->
            red + green + blue
