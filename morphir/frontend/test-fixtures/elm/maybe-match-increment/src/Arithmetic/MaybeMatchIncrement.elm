module Arithmetic.MaybeMatchIncrement exposing (maybeMatchIncrement)

maybeMatchIncrement : Maybe Int -> Int
maybeMatchIncrement maybeValue =
    case maybeValue of
        Just value ->
            value + 1

        Nothing ->
            0
