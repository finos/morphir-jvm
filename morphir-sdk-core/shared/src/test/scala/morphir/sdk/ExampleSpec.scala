package morphir.sdk

import zio.test._
import zio.test.Assertion._
import zio.test.AssertionM.Render.param

object ExampleSpec extends DefaultRunnableSpec {

  def spec = suite("Example Spec")(
    test("matching elements")(
      assert(List(1, 2, 3, 4))(hasMatchingElements(List(1, 2, 3, 4))(Assertion.assertionDirect("elementsMatch")() {
        case (actual, expected) => Assertion.equalTo(expected).apply(actual)
      }))
    )
//    testM("matching elements")(
//      for {
//        actual   <- ZIO.succeed(List(2, 4, 6, 8))
//        expected <- ZIO.succeed(List(1, 2, 3, 4))
//        pairs     = actual.zip(expected)
//      } yield assert(pairs)(forall(Assertion.assertionDirect("elementsMatch")() { case (actual, expected) =>
//        Assertion.equalTo(expected).apply(actual)
//      }))
//    )
  )

  def hasMatchingElements[A](elements: Iterable[A])(assertion: Assertion[(A, A)]): Assertion[Iterable[A]] =
    Assertion.assertionRec[Iterable[A], (A, A)]("hasMatchingElements")(param(elements), param(assertion))(assertion)(
      (actual) =>
        if (actual.size != elements.size) None
        else actual.zip(elements).find { case (l, r) => assertion.run((l, r)).isFailure },
      _.asSuccess
    ) && hasSize(equalTo(elements.size))
}
