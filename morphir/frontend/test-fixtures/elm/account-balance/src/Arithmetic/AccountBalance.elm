module Arithmetic.AccountBalance exposing (AccountBalance)

import Morphir.SDK.Decimal as Decimal

type alias AccountBalance =
    { amount : Decimal.Decimal
    , isOverdue : Bool
    }
