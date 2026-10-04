module Arithmetic.DecimalGt exposing (decimalGt)

import Morphir.SDK.Decimal as Decimal

decimalGt : Decimal.Decimal -> Decimal.Decimal -> Bool
decimalGt a b =
    Decimal.gt a b
