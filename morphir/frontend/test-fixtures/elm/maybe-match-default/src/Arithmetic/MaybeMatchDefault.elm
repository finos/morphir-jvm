module Arithmetic.MaybeMatchDefault exposing (maybeMatchDefault)

maybeMatchDefault : Int -> Maybe Int -> Int
maybeMatchDefault fallback maybeValue =
    case maybeValue of
        Just value ->
            value

        Nothing ->
            fallback
