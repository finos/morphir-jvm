package zio.morphir.ir

object Source {
  type Located[+A] = source.Located[A]
  val Located: source.Located.type = source.Located

  type Location = source.Location
  val Location: source.Location.type = source.Location

  type Region = source.Region
  val Region: source.Region.type = source.Region
}
