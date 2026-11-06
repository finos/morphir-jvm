package unsupported

object GuardedMatch:
  def guarded(value: Int): Int = value match
    case n if n > 0 => n
    case _          => 0
