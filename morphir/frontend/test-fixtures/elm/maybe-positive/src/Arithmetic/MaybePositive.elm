module Arithmetic.MaybePositive exposing (maybePositive)

maybePositive : Int -> Maybe Int
maybePositive a =
    if a > 0 then
        Just a

    else
        Nothing
