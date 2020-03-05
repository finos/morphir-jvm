package com.morganstanley.morphir.ir.advanced
import com.morganstanley.morphir.ir.FQName

object value {
  sealed abstract class Value[+X]
  object Value {
    type Lit = com.morganstanley.morphir.ir.advanced.value.Literal

    case class Literal[X](value: Lit, extra: X) extends Value[X]
    case class Constructor[X](fullyQualifiedName: FQName, extra: X)
        extends Value[X]
  }

  sealed abstract class Literal {
    def isBoolLiteral: Boolean = false
    def isCharLiteral: Boolean = false
    def isStringLiteral: Boolean = false
    def isIntLiteral: Boolean = false
    def isFloatLiteral: Boolean = false
  }

  object Literal {
    case class BoolLiteral(value: Boolean) extends Literal {
      override def isBoolLiteral: Boolean = true
    }
    case class CharLiteral(value: Char) extends Literal {
      override def isCharLiteral: Boolean = true
    }
    case class StringLiteral(value: String) extends Literal {
      override def isStringLiteral: Boolean = true
    }
    case class IntLiteral(value: Int) extends Literal {
      override def isIntLiteral: Boolean = true
    }
    case class FloatLiteral(value: Float) extends Literal {
      override def isFloatLiteral: Boolean = true
    }
  }
}
