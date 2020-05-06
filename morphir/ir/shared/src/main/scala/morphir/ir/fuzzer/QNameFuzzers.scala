package morphir.ir.fuzzer

import morphir.ir.{ Name, Path, QName }

trait QNameFuzzers {
  implicit def fuzzQName(implicit pathFuzzer: Fuzzer[Path], nameFuzzer: Fuzzer[Name]): Fuzzer[QName] =
    for {
      path      <- pathFuzzer
      localName <- nameFuzzer
    } yield QName(path, localName)

}

object QNameFuzzers extends QNameFuzzers
