module Arithmetic.DecimalAdd exposing (decimalAdd)

import Morphir.SDK.Decimal as Decimal

decimalAdd : Decimal.Decimal -> Decimal.Decimal -> Decimal.Decimal
decimalAdd a b =
    Decimal.add a b
