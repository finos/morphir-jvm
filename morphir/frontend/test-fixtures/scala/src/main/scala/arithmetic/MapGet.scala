package arithmetic

object MapGet:
  def find(key: String, values: Map[String, Int]): Option[Int] =
    values.get(key)
