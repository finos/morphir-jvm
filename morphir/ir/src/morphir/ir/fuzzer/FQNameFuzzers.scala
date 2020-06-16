package morphir.ir.fuzzer

import morphir.ir.name.Name
import morphir.ir.path.Path
import morphir.ir.FQName

trait FQNameFuzzers {
  implicit def fuzzFQName(implicit pathFuzzer: Fuzzer[Path], nameFuzzer: Fuzzer[Name]): Fuzzer[FQName] =
    for {
      packagePath <- pathFuzzer
      modulePath  <- pathFuzzer
      localName   <- nameFuzzer
    } yield FQName(packagePath, modulePath, localName)
}

object FQNameFuzzers extends FQNameFuzzers
