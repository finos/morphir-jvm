module Arithmetic.DecimalLte exposing (decimalLte)

import Morphir.SDK.Decimal as Decimal

decimalLte : Decimal.Decimal -> Decimal.Decimal -> Bool
decimalLte a b =
    Decimal.lte a b
