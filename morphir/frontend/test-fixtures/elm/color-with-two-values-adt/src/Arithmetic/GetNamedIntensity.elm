module Arithmetic.GetNamedIntensity exposing (getNamedIntensity)

import Arithmetic.ColorWithTwoValues exposing (ColorWithTwoValues(..))


getNamedIntensity : ColorWithTwoValues -> Int
getNamedIntensity color =
    case color of
        Named _ intensity ->
            intensity

        Custom red green ->
            red + green
