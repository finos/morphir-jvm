package morphir.ir.fuzzer

import morphir.ir.name.Name
import morphir.ir.path.Path
import morphir.ir.QName

trait QNameFuzzers {
  implicit def fuzzQName(implicit pathFuzzer: Fuzzer[Path], nameFuzzer: Fuzzer[Name]): Fuzzer[QName] =
    for {
      path      <- pathFuzzer
      localName <- nameFuzzer
    } yield QName(path, localName)

}

object QNameFuzzers extends QNameFuzzers
