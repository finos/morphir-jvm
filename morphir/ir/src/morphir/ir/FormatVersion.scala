package morphir.ir

/** Generated based on IR.FormatVersion
 */
object FormatVersion{

  final case class VersionedDistribution(
                                          formatVersion: morphir.sdk.Basics.Int,
                                          distribution: morphir.ir.Distribution.Distribution
                                        ){}

  def currentFormatVersion: morphir.sdk.Basics.Int =
    morphir.sdk.Basics.Int(3)

}