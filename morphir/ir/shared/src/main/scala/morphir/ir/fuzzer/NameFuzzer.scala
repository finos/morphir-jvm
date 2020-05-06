package morphir.ir.fuzzer

import morphir.ir.Name
import zio.test.Gen

trait NameFuzzer {
  val fuzzName: Gen[zio.random.Random with zio.test.Sized, Name] = {
    val fuzzWord = {
      val choices =
        Seq(
          "area",
          "benchmark",
          "book",
          "business",
          "company",
          "country",
          "currency",
          "day",
          "description",
          "entity",
          "fact",
          "family",
          "from",
          "government",
          "group",
          "home",
          "id",
          "job",
          "left",
          "lot",
          "market",
          "minute",
          "money",
          "month",
          "name",
          "number",
          "owner",
          "parent",
          "part",
          "problem",
          "rate",
          "right",
          "state",
          "source",
          "system",
          "time",
          "title",
          "to",
          "valid",
          "week",
          "work",
          "world",
          "year"
        ).map(Gen.const(_))
      Gen.oneOf(choices: _*)
    }

    Gen.listOf(fuzzWord).map(lst => lst.take(3)).map(Name.fromList)
  }
}

object NameFuzzer extends NameFuzzer
