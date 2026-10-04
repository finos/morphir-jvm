module Arithmetic.ListEmpty exposing (defaultList, emptyViaFactory, emptyViaNil)

emptyViaFactory : List Int
emptyViaFactory =
    []


emptyViaNil : List Int
emptyViaNil =
    []


defaultList : Maybe (List Int) -> List Int
defaultList input =
    case input of
        Just values ->
            values

        Nothing ->
            []
