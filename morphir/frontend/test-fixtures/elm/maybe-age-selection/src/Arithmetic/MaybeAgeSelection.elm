module Arithmetic.MaybeAgeSelection exposing (selectedAge)

import Arithmetic.MaybeAgeSelectionRecord exposing (MaybeAgeSelectionRecord)

selectedAge : MaybeAgeSelectionRecord -> Maybe Int
selectedAge record =
    if record.enabled then
        record.age

    else
        Nothing
