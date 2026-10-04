module Arithmetic.DecimalLt exposing (decimalLt)

import Morphir.SDK.Decimal as Decimal

decimalLt : Decimal.Decimal -> Decimal.Decimal -> Bool
decimalLt a b =
    Decimal.lt a b
