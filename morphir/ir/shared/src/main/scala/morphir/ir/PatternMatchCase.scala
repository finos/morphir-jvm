package morphir.ir

object PatternMatchCase {
  def apply[A](pattern: Pattern[A], value: Value[A]): PatternMatchCase[A] = (pattern, value)
}
