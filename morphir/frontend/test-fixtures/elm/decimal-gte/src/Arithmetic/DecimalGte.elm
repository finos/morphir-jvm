module Arithmetic.DecimalGte exposing (decimalGte)

import Morphir.SDK.Decimal as Decimal

decimalGte : Decimal.Decimal -> Decimal.Decimal -> Bool
decimalGte a b =
    Decimal.gte a b
