package zio.morphir.ir.source
import zio.prelude._
final case class Region(start: Location, end: Location)
object Region {
  val default: Region = Region(Location.default, Location.default)
  implicit val RegionIdentity: Identity[Region] = new Identity[Region] {
    lazy val identity: Region = default

    override def combine(l: => Region, r: => Region): Region = Region(l.start <> r.start, l.end <> r.end)
  }
}
