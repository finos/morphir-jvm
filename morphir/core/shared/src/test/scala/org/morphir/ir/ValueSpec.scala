package org.morphir.ir

import zio.test.Assertion.{isFalse, isTrue}
import zio.test._

object ValueSpec extends DefaultRunnableSpec {
  def spec = suite("ValueSpec")(
    suite("Literals")(
      suite("Given SUT is a BoolLiteral")(
        testM("Then SUT.isBoolLiteral should be true") {
          check(Gen.boolean) { input =>
            val sut = Literal.BoolLiteral(input)
            assert(sut.isBoolLiteral)(isTrue)
          }
        },
        testM("Then SUT.isCharLiteral should be false") {
          check(Gen.boolean) { input =>
            val sut = Literal.BoolLiteral(input)
            assert(sut.isCharLiteral)(isFalse)
          }
        },
        testM("Then SUT.isFloatLiteral should be false") {
          check(Gen.boolean) { input =>
            val sut = Literal.BoolLiteral(input)
            assert(sut.isFloatLiteral)(isFalse)
          }
        },
        testM("Then SUT.isIntLiteral should be false") {
          check(Gen.boolean) { input =>
            val sut = Literal.BoolLiteral(input)
            assert(sut.isIntLiteral)(isFalse)
          }
        },
        testM("Then SUT.isStringLiteral should be false") {
          check(Gen.boolean) { input =>
            val sut = Literal.BoolLiteral(input)
            assert(sut.isStringLiteral)(isFalse)
          }
        }
      ),
      suite("Given SUT is a CharLiteral")(
        testM("Then SUT.isBoolLiteral should be false") {
          check(Gen.anyChar) { input =>
            val sut = Literal.CharLiteral(input)
            assert(sut.isBoolLiteral)(isFalse)
          }
        },
        testM("Then SUT.isCharLiteral should be false") {
          check(Gen.anyChar) { input =>
            val sut = Literal.CharLiteral(input)
            assert(sut.isCharLiteral)(isTrue)
          }
        },
        testM("Then SUT.isFloatLiteral should be false") {
          check(Gen.anyChar) { input =>
            val sut = Literal.CharLiteral(input)
            assert(sut.isFloatLiteral)(isFalse)
          }
        },
        testM("Then SUT.isIntLiteral should be false") {
          check(Gen.anyChar) { input =>
            val sut = Literal.CharLiteral(input)
            assert(sut.isIntLiteral)(isFalse)
          }
        },
        testM("Then SUT.isStringLiteral should be false") {
          check(Gen.anyChar) { input =>
            val sut = Literal.CharLiteral(input)
            assert(sut.isStringLiteral)(isFalse)
          }
        }
      ),
      suite("Given SUT is a FloatLiteral")(
        testM("Then SUT.isBoolLiteral should be false") {
          check(Gen.anyFloat) { input =>
            val sut = Literal.FloatLiteral(input)
            assert(sut.isBoolLiteral)(isFalse)
          }
        },
        testM("Then SUT.isCharLiteral should be false") {
          check(Gen.anyFloat) { input =>
            val sut = Literal.FloatLiteral(input)
            assert(sut.isCharLiteral)(isFalse)
          }
        },
        testM("Then SUT.isFloatLiteral should be false") {
          check(Gen.anyFloat) { input =>
            val sut = Literal.FloatLiteral(input)
            assert(sut.isFloatLiteral)(isTrue)
          }
        },
        testM("Then SUT.isIntLiteral should be false") {
          check(Gen.anyFloat) { input =>
            val sut = Literal.FloatLiteral(input)
            assert(sut.isIntLiteral)(isFalse)
          }
        },
        testM("Then SUT.isStringLiteral should be false") {
          check(Gen.anyFloat) { input =>
            val sut = Literal.FloatLiteral(input)
            assert(sut.isStringLiteral)(isFalse)
          }
        }
      ),
      suite("Given SUT is a IntLiteral")(
        testM("Then SUT.isBoolLiteral should be false") {
          check(Gen.anyInt) { input =>
            val sut = Literal.IntLiteral(input)
            assert(sut.isBoolLiteral)(isFalse)
          }
        },
        testM("Then SUT.isCharLiteral should be false") {
          check(Gen.anyInt) { input =>
            val sut = Literal.IntLiteral(input)
            assert(sut.isCharLiteral)(isFalse)
          }
        },
        testM("Then SUT.isFloatLiteral should be false") {
          check(Gen.anyInt) { input =>
            val sut = Literal.IntLiteral(input)
            assert(sut.isFloatLiteral)(isFalse)
          }
        },
        testM("Then SUT.isIntLiteral should be false") {
          check(Gen.anyInt) { input =>
            val sut = Literal.IntLiteral(input)
            assert(sut.isIntLiteral)(isTrue)
          }
        },
        testM("Then SUT.isStringLiteral should be false") {
          check(Gen.anyInt) { input =>
            val sut = Literal.IntLiteral(input)
            assert(sut.isStringLiteral)(isFalse)
          }
        }
      ),
      suite("Given SUT is a StringLiteral")(
        testM("Then SUT.isBoolLiteral should be false") {
          check(Gen.anyString) { input =>
            val sut = Literal.StringLiteral(input)
            assert(sut.isBoolLiteral)(isFalse)
          }
        },
        testM("Then SUT.isCharLiteral should be false") {
          check(Gen.anyString) { input =>
            val sut = Literal.StringLiteral(input)
            assert(sut.isCharLiteral)(isFalse)
          }
        },
        testM("Then SUT.isFloatLiteral should be false") {
          check(Gen.anyString) { input =>
            val sut = Literal.StringLiteral(input)
            assert(sut.isFloatLiteral)(isFalse)
          }
        },
        testM("Then SUT.isIntLiteral should be false") {
          check(Gen.anyString) { input =>
            val sut = Literal.StringLiteral(input)
            assert(sut.isIntLiteral)(isFalse)
          }
        },
        testM("Then SUT.isStringLiteral should be false") {
          check(Gen.anyString) { input =>
            val sut = Literal.StringLiteral(input)
            assert(sut.isStringLiteral)(isTrue)
          }
        }
      )
    )
  )
}
