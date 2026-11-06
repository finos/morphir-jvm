module Arithmetic.Clamp exposing (clamp)

clamp : Int -> Int -> Int -> Int
clamp low high a =
    if a < low then
        low

    else if a > high then
        high

    else
        a
