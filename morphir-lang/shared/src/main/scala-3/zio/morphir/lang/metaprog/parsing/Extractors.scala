package zio.morphir.lang.metaprog.parsing
import scala.quoted._

object Extractors {
  // Whether you are unapplying this or applying this to an Expr, the functionality is the same.
  // Take a Expr that has a type ascription (e.g. '{ blah: BlahType }) and remove the type ascription
  // if such an ascription exists (note that there could be more then one e.g.
  // '{ ((blah: BlahType): BlahType) } ). If there are no type ascriptions, just return the term.
  // The unapply allows it to be done inside of a matcher.
  object UntypeExpr {
    def unapply(using Quotes)(expr: Expr[_]): Option[Expr[_]] =
      import quotes.reflect._
      Untype.unapply(expr.asTerm).map(_.asExpr)

    def apply(using Quotes)(expr: Expr[_]): Expr[_] =
      import quotes.reflect._
      import scala.util.{Try, Success, Failure}
      Untype.unapply(expr.asTerm).map(_.asExpr).get
  }

  // Always match (whether ast starts with Typed or not). If it does, strip the Typed node.
  object Untype {
    def unapply(using Quotes)(term: quotes.reflect.Term): Option[quotes.reflect.Term] = term match {
      case TypedMatroshkaTerm(t) => Some(t)
      case other                 => Some(other)
    }

    def apply(using Quotes)(term: quotes.reflect.Term) = Untype.unapply(term).get
  }

  object TypedMatroshkaTerm {
    def recurse(using Quotes)(innerTerm: quotes.reflect.Term): quotes.reflect.Term =
      import quotes.reflect._
      innerTerm match
        case Typed(innerTree, _) => recurse(innerTree)
        case other               => other

    def unapply(using Quotes)(term: quotes.reflect.Term): Option[quotes.reflect.Term] =
      import quotes.reflect._
      term match
        case Typed(tree, _) => Some(recurse(tree))
        case other          => None
  }

  object TypedMatroshka {
    def unapply(using Quotes)(term: Expr[Any]): Option[Expr[Any]] =
      import quotes.reflect._
      TypedMatroshkaTerm.unapply(term.asTerm).map(_.asExpr)
  }
}
