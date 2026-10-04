package arithmetic

case class MixedRecord(
  name: String,
  score: Float,
  amount: BigDecimal,
  maybeAge: Option[Int],
  isActive: Boolean
)
