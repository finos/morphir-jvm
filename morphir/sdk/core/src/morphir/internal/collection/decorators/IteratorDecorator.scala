package morphir.internal.collection.decorators

/** Enriches Iterator with additional methods. */
class IteratorDecorator[A](val `this`: Iterator[A]) extends AnyVal {

  def foldSomeLeft[B](z: B)(op: (B, A) => Option[B]): B = {
    var result: B = z
    while (`this`.hasNext) {
      op(result, `this`.next()) match {
        case Some(v) => result = v
        case None    => return result
      }
    }
    result
  }
}
