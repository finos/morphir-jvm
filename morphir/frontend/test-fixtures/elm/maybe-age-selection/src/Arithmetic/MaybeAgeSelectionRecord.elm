module Arithmetic.MaybeAgeSelectionRecord exposing (MaybeAgeSelectionRecord)

type alias MaybeAgeSelectionRecord =
    { enabled : Bool
    , age : Maybe Int
    }
