package morphir.flowz

final case class FilteredRecord[Record](record: Record, reason: Option[String])

final case class EnrichmentFailure[Orig](originalRecord: Orig, reason: Option[String])
