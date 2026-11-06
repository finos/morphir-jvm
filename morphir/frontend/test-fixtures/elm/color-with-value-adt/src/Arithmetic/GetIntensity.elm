module Arithmetic.GetIntensity exposing (getIntensity)

import Arithmetic.ColorWithValue exposing (ColorWithValue(..))

getIntensity : ColorWithValue -> Int
getIntensity color =
    case color of
        Red i ->
            i

        Blue s ->
            s * 2
