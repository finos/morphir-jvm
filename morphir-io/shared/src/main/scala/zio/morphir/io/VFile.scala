package zio.morphir.io
import zio.*
import zio.prelude.*
import scala.language.postfixOps

/**
 * This is meant to pretty much be an implementation of this: https://www.npmjs.com/package/vfile#vfiledata
 */
sealed trait VFile[-R, +E, +T] { self =>
  import VFile.*
  import VFileCase.*

  def $case: VFileCase[R, E, T, VFile[R, E, T]]

  def fold[Z](f: VFileCase[R, E, T, Z] => Z): Z = self.$case match {
    case c @ ContentCase(_, _)  => f(ContentCase(c.child.map(_.fold(f)), c.data))
    case c @ FileCase(_, _)     => f(FileCase(c.path, c.children.map(_.map(_.fold(f)))))
    case c @ LineCase(_, _)     => f(LineCase(c.index, c.text))
    case c @ TextContentCase(_) => f(TextContentCase(c.text))
  }
//
//     // def useIt = foldAttributed {
//     //   case c @ FileCase(_, _) =>
//     //     val attrib = c.children.head
//     //     ???
//     //   case c @ LineCase(_, _) => ???
//     // }
//
//     // /** Folds over the recursive data structure to reduce it to a summary value, providing access to the recursive
//     //   * structure annotated with the current previous summary values in each step of the fold.
//     //   */
//     // def foldAttributed[Z](
//
//     //     f: VFileCase[Properties[VFileCase, Z]] => Properties[VFileCase, Z]
//     // ): Properties[VFileCase, Z] = {
//     //   def annotate(recursive: VFile): Properties[VFileCase, Z] =
//     //     Properties(recursive.$case.map(annotate), recursive.foldAttributed(f))
//     //   f($case.map(annotate))
//     // }
}
object VFile {
//     // final case class File[+Props]($case: VFileCase[VFile]) extends VFile {}
//
  sealed trait VFileCase[-R, +E, +D, +Self] { self =>
    import VFileCase.*
    def map[Self2](f: Self => Self2): VFileCase[R, E, D, Self2] = self match {
      case c @ FileCase(_, _)     => FileCase(c.path, c.children.map(_.map(f)))
      case c @ LineCase(_, text)  => LineCase(c.index, text)
      case c @ ContentCase(_, _)  => ContentCase(c.child.map(f), c.data)
      case c @ TextContentCase(_) => TextContentCase(c.text)
    }
  }

  object VFileCase {

    final case class FileCase[-R, +E, +Self](path: VFilePath, children: ZIO[R, E, Chunk[Self]])
        extends VFileCase[R, E, Nothing, Self]
    final case class LineCase(index: Int, text: String) extends VFileCase[Any, Nothing, Nothing, Nothing]
    final case class ContentCase[-R, +E, +T, +Self](child: ZIO[R, E, Self], data: T) extends VFileCase[R, E, T, Self]
    final case class TextContentCase(text: String) extends VFileCase[Any, Nothing, Nothing, Nothing]
  }

//   final case class VFileMessage(reason: VFileMessage.Reason)
//   object VFileMessage:
//     enum Reason:
//       case Description(text: String)
//       case Error(value: Throwable)
//   end VFileMessage
//
//   object LineNumber extends Newtype[Int]:
//     import zio.prelude.Assertion.*
//
//     extension (self: LineNumber) def ++ = wrap(unwrap(self) + 1)
//
//     val default: LineNumber       = wrap(1)
//     override inline def assertion = greaterThanOrEqualTo(1)
//
//   type LineNumber = LineNumber.Type
//
//   opaque type ColumnNumber = Int
//
//   final case class Offset(line: Option[LineNumber], column: Option[ColumnNumber])
//   object Offset:
//     def apply(line: LineNumber)                       = new Offset(Some(line), None)
//     def apply(line: LineNumber, column: ColumnNumber) = new Offset(Some(line), Some(column))
//     object Empty:
//       def apply: Offset = Offset(None, None)
//       def unapply(offset: Offset): Option[Unit] = offset match
//         case Offset(None, None) => Some(())
//         case _                  => None
//
//   final case class Position(start: Option[Offset], end: Option[Offset])
//   object Position:
//     def apply(start: Offset) = new Position(Some(start), None)
}
