package morphir.ir.fuzzer

import morphir.ir.name.Name
import morphir.ir.path.Path
import zio.random.Random
import zio.test.{ Gen, Sized }

trait PathFuzzers {
  implicit def fuzzPath(implicit nameFuzzer: Gen[Random with Sized, Name]): Gen[Random with Sized, Path] =
    Gen.listOf(nameFuzzer).map(l => l.take(3)).map(Path.fromList)
}

object PathFuzzers extends PathFuzzers
