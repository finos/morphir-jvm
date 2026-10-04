module Arithmetic.DecimalMultiply exposing (decimalMultiply)

import Morphir.SDK.Decimal as Decimal

decimalMultiply : Decimal.Decimal -> Decimal.Decimal -> Decimal.Decimal
decimalMultiply a b =
    Decimal.mul a b
