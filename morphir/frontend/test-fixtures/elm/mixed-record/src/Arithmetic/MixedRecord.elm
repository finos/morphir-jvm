module Arithmetic.MixedRecord exposing (MixedRecord)

import Morphir.SDK.Decimal as Decimal

type alias MixedRecord =
    { name : String
    , score : Float
    , amount : Decimal.Decimal
    , maybeAge : Maybe Int
    , isActive : Bool
    }
