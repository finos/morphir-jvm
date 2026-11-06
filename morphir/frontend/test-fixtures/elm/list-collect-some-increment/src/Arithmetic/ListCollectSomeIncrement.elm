module Arithmetic.ListCollectSomeIncrement exposing (collectDefined)

collectDefined : List (Maybe Int) -> List Int
collectDefined values =
    List.filterMap
        (\x1 ->
            case x1 of
                Just value ->
                    Just (value + 1)

                _ ->
                    Nothing
        )
        values
