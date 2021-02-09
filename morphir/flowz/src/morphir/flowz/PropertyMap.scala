package morphir.flowz

/**
 * A property map keeps track of properties of different types.
 * It serves as a backing store for properties and can be used to extend/addProperty objects with custom properties.
 */
final class PropertyMap private (private val map: Map[Property[Any], AnyRef]) { self =>

  def ++(that: PropertyMap): PropertyMap =
    new PropertyMap((self.map.toVector ++ that.map.toVector).foldLeft[Map[Property[Any], AnyRef]](Map()) {
      case (acc, (key, value)) =>
        acc + (key -> acc.get(key).fold(value)(key.combine(_, value).asInstanceOf[AnyRef]))
    })

  /**
   * Appends the specified annotation to the annotation map.
   */
  def annotate[V](key: Property[V], value: V): PropertyMap = {
    val res = update[V](key, key.combine(_, value))
    res
  }

  /**
   * Retrieves the annotation of the specified type, or its default value if there is none.
   */
  def get[V](key: Property[V]): V =
    map.get(key.asInstanceOf[Property[Any]]).fold(key.initial)(_.asInstanceOf[V])

  private def overwrite[V](key: Property[V], value: V): PropertyMap =
    new PropertyMap(map + (key.asInstanceOf[Property[Any]] -> value.asInstanceOf[AnyRef]))

  private def update[V](key: Property[V], f: V => V): PropertyMap =
    overwrite(key, f(get(key)))

  override def toString: String = map.toString()
}

object PropertyMap {

  /**
   * An empty annotation map.
   */
  val empty: PropertyMap = new PropertyMap(Map())
}
