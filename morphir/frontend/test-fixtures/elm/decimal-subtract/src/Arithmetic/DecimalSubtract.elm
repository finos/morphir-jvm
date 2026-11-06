module Arithmetic.DecimalSubtract exposing (decimalSubtract)

import Morphir.SDK.Decimal as Decimal

decimalSubtract : Decimal.Decimal -> Decimal.Decimal -> Decimal.Decimal
decimalSubtract a b =
    Decimal.sub a b
