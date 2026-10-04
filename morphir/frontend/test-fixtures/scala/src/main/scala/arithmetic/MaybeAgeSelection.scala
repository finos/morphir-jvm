package arithmetic

object MaybeAgeSelection:
  def selectedAge(record: MaybeAgeSelectionRecord): Option[Int] =
    if record.enabled then record.age
    else None
