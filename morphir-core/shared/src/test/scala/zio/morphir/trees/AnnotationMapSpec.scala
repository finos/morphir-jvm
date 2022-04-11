package zio.morphir.trees

import zio.test._

object AnnotationMapSpec extends ZIOSpecDefault {
  def spec: ZSpec[Environment, Any] = suite("AnnotationMap")(
    test("Should support combining annotations") {
      val sut = AnnotationMap.empty.annotate(total, 10).annotate(total, 20).annotate(total, 30)
      assertTrue(sut.get(total) == 60)
    },
    test("Should support combining annotation maps") {
      val map1   = AnnotationMap.empty.annotate(total, 10).annotate(names, List("Andy", "Becky"))
      val map2   = AnnotationMap.empty.annotate(total, 20).annotate(names, List("Bob", "Carol"))
      val actual = map1 ++ map2
      assertTrue(actual.get(total) == 30, actual.get(names) == List("Andy", "Becky", "Bob", "Carol"))
    }
  )

  val total: Annotation[Int]          = Annotation[Int]("total", 0, _ + _)
  val names: Annotation[List[String]] = Annotation[List[String]]("names", List.empty[String], _ ++ _)
}
