package morphir.flowz

/**
 * An annotation map keeps track of annotations of different types.
 */
final class StepAnnotationMap private (private val map: Map[StepAnnotation[Any], AnyRef]) { self =>

  def ++(that: StepAnnotationMap): StepAnnotationMap =
    new StepAnnotationMap((self.map.toVector ++ that.map.toVector).foldLeft[Map[StepAnnotation[Any], AnyRef]](Map()) {
      case (acc, (key, value)) =>
        acc + (key -> acc.get(key).fold(value)(key.combine(_, value).asInstanceOf[AnyRef]))
    })

  /**
   * Appends the specified annotation to the annotation map.
   */
  def annotate[V](key: StepAnnotation[V], value: V): StepAnnotationMap = {
    val res = update[V](key, key.combine(_, value))
    res
  }

  /**
   * Retrieves the annotation of the specified type, or its default value if there is none.
   */
  def get[V](key: StepAnnotation[V]): V =
    map.get(key.asInstanceOf[StepAnnotation[Any]]).fold(key.initial)(_.asInstanceOf[V])

  private def overwrite[V](key: StepAnnotation[V], value: V): StepAnnotationMap =
    new StepAnnotationMap(map + (key.asInstanceOf[StepAnnotation[Any]] -> value.asInstanceOf[AnyRef]))

  private def update[V](key: StepAnnotation[V], f: V => V): StepAnnotationMap =
    overwrite(key, f(get(key)))

  override def toString: String = map.toString()
}

object StepAnnotationMap {

  /**
   * An empty annotation map.
   */
  val empty: StepAnnotationMap = new StepAnnotationMap(Map())
}
