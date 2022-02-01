package zio.morphir.trees

final class AnnotationMap[+R] private (private val map: Map[Annotation[_], _]) {
  self =>

  def ++[R1 >: R](that: AnnotationMap[R1]): AnnotationMap[R1] =
    new AnnotationMap(
      (self.map.toVector ++ that.map.toVector).foldLeft[Map[Annotation[_], _]](Map()) { case (acc, (ann, value)) =>
        acc + (ann -> acc
          .get(ann)
          .fold(value)(v2 => ann.unsafeCombine(v2, value)))
      }
    )

  /**
   * Appends the specified annotation to the annotation map.
   */
  def annotate[V](key: Annotation[V], value: V): AnnotationMap[R with V] = {
    val res = update[V](key, key.combine(_, value))
    res
  }

  def unannotate[V](key: Annotation[V])(implicit ev: R <:< V): AnnotationMap[R] =
    new AnnotationMap(map - key)

  /**
   * Retrieves the annotation of the specified type, or its default value if there is none.
   */
  def get[V](key: Annotation[V])(implicit ev: R <:< V): V =
    internalGet(key)

  private def internalGet[V](key: Annotation[V]): V =
    map.get(key.asInstanceOf[Annotation[Any]]).fold(key.initial)(_.asInstanceOf[V])

  private def overwrite[V](key: Annotation[V], value: V): AnnotationMap[R with V] =
    new AnnotationMap(map + (key.asInstanceOf[Annotation[Any]] -> value.asInstanceOf[AnyRef]))

  private def update[V](key: Annotation[V], f: V => V): AnnotationMap[R with V] =
    overwrite(key, f(internalGet(key)))

  override def toString: String =
    map.toString
}

object AnnotationMap {
  val empty: AnnotationMap[Any] = new AnnotationMap(Map())
}
