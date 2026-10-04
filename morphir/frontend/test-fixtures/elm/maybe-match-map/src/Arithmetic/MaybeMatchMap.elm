module Arithmetic.MaybeMatchMap exposing (maybeMatchMap)

maybeMatchMap : Maybe Int -> Maybe Int
maybeMatchMap maybeValue =
    case maybeValue of
        Just value ->
            Just (value + 1)

        Nothing ->
            Nothing
