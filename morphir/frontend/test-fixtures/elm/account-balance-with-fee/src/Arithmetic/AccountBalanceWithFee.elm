module Arithmetic.AccountBalanceWithFee exposing (addFee)

import Arithmetic.AccountBalanceWithFeeRecord exposing (AccountBalanceWithFeeRecord)
import Morphir.SDK.Decimal as Decimal

addFee : AccountBalanceWithFeeRecord -> Decimal.Decimal -> Decimal.Decimal
addFee account fee =
    Decimal.add account.balance fee
