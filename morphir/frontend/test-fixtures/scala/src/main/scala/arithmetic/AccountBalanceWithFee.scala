package arithmetic

object AccountBalanceWithFee:
  def addFee(account: AccountBalanceWithFeeRecord, fee: BigDecimal): BigDecimal =
    account.balance + fee
