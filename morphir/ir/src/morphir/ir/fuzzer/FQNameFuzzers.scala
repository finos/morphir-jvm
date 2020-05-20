package morphir.ir.fuzzer

import morphir.ir.{ FQName, Name, Path }

trait FQNameFuzzers {
  implicit def fuzzFQName(implicit pathFuzzer: Fuzzer[Path], nameFuzzer: Fuzzer[Name]): Fuzzer[FQName] =
    for {
      packagePath <- pathFuzzer
      modulePath  <- pathFuzzer
      localName   <- nameFuzzer
    } yield FQName(packagePath, modulePath, localName)
}

object FQNameFuzzers extends FQNameFuzzers
