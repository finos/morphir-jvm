package unsupported

object SequencePattern:
  def first(values: List[Int]): Int = values match
    case List(head, _*) => head
    case _              => 0
