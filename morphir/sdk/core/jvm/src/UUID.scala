import memeid4s.{ UUID => MUUID }

import java.util.{ UUID => JUUID }
import scala.util.Try

object UUID {
  type UUID = MUUID

  val Nil: UUID = MUUID.Nil

  type V1 = MUUID.V1

  type V2 = MUUID.V2

  type V3 = MUUID.V3

  type V4 = MUUID.V4

  type V5 = MUUID.V5

  val dnsNamespace  = "6ba7b810-9dad-11d1-80b4-00c04fd430c8"
  val urlNamespace  = "6ba7b811-9dad-11d1-80b4-00c04fd430c8"
  val oidNamespace  = "6ba7b812-9dad-11d1-80b4-00c04fd430c8"
  val x500Namespace = "6ba7b814-9dad-11d1-80b4-00c04fd430c8"

  object V1 {
    def next(): UUID = MUUID.V1.next
  }
  object V3 {
    def apply(namespace: UUID, local: String): UUID = MUUID.V3(namespace, local)
  }
  object V4 {
    def random: UUID = MUUID.V4.random
  }
  object V5 {
    def apply(namespace: UUID, local: String): UUID = MUUID.V3(namespace, local)
  }

  /** Creates a valid [[UUID]] from two [[_root_.scala.Long Long]] values representing the most/least significant bits.
    *
    * @param msb
    *   Most significant bit in [[_root_.scala.Long Long]] representation
    * @param lsb
    *   Least significant bit in [[_root_.scala.Long Long]] representation
    * @return
    *   a new [[UUID]] constructed from msb and lsb
    */
  @inline def from(msb: Long, lsb: Long): UUID = MUUID.from(msb, lsb)

  /** Creates a [[UUID UUID]] from the [[java.util.UUID#toString string standard representation]] wrapped in a
    * [[_root_.scala.util.Right Right]].
    *
    * Returns [[_root_.scala.util.Left Left]] with the error in case the string doesn't follow the string standard
    * representation.
    *
    * @param s
    *   String for the [[java.util.UUID UUID]] to be generated as an [[UUID]]
    * @return
    *   [[_root_.scala.util.Either Either]] with [[_root_.scala.util.Left Left]] with the error in case the string
    *   doesn't follow the string standard representation or [[_root_.scala.util.Right Right]] with the [[UUID UUID]]
    *   representation.
    */
  @inline def from(s: String): Either[Throwable, UUID] = MUUID.from(s)

  /** Creates a valid [[UUID]] from a [[JUUID]].
    *
    * @param juuid
    *   the { @link java.util.UUID}
    * @return
    *   a valid { @link UUID} created from a { @link java.util.UUID}
    */
  @inline def fromUUID(juuid: JUUID): UUID = MUUID.fromUUID(juuid)

  /** Returns true if the given string represents the Nil UUID
    *
    * @param s
    *   the string to check
    * @return
    *   boolean indicating if the string represents the Nil UUID
    */
  @inline def isNil(s: String): Boolean = Nil.toString.equals(s)

}
