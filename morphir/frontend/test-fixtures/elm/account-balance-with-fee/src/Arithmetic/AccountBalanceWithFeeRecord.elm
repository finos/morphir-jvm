module Arithmetic.AccountBalanceWithFeeRecord exposing (AccountBalanceWithFeeRecord)

import Morphir.SDK.Decimal as Decimal

type alias AccountBalanceWithFeeRecord =
    { balance : Decimal.Decimal
    }
