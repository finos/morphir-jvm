// package zio.morphir.ir
// import zio.*
// import zio.prelude.*
// import scala.language.postfixOps

// final case class Attributed[Case[+_], +A](
//     caseValue: Case[Attributed[Case, A]],
//     attributes: ZEnvironment[A]
// )
// final case class Properties[Case[+_], +Props](env: ZEnvironment[Props]) {}

// sealed trait Property {}

// object Property {
//   case class LineCount(count: Int) extends Property
//   case class FileCount(count: Int) extends Property
// }
// object Properties {

//   def lines(count: Int) = ZEnvironment.empty.add(Property.LineCount(count))
//   def file(count: Int)  = ZEnvironment.empty.add(Property.FileCount(count))
// }

// /**
//  * This is meant to pretty much be an implementation of this:
//  * https://www.npmjs.com/package/vfile#vfiledata
//  */
// object vfile:
//   sealed trait VFile[-R, +E, +D] { self =>
//     import VFile.*
//     import VFileCase.*

//     def $case: VFileCase[R, E, VFile[R, E, D], D]

//     // def useIt = foldAttributed {
//     //   case c @ FileCase(_, _) =>
//     //     val attrib = c.children.head
//     //     ???
//     //   case c @ LineCase(_, _) => ???
//     // }

//     // /** Folds over the recursive data structure to reduce it to a summary value, providing access to the recursive
//     //   * structure annotated with the current previous summary values in each step of the fold.
//     //   */
//     // def foldAttributed[Z](
//     //     f: VFileCase[Properties[VFileCase, Z]] => Properties[VFileCase, Z]
//     // ): Properties[VFileCase, Z] = {
//     //   def annotate(recursive: VFile): Properties[VFileCase, Z] =
//     //     Properties(recursive.$case.map(annotate), recursive.foldAttributed(f))
//     //   f($case.map(annotate))
//     // }
//   }
//   object VFile:
//     // final case class File[+Props]($case: VFileCase[VFile]) extends VFile {}

//     sealed trait VFileCase[-R, +E, +A, +D]:
//       self =>
//       import VFileCase.*
//       def map[B](f: A => B): VFileCase[R, E, B, D] = self match
//         case c @ FileCase(_, _)    => FileCase(c.path, c.children.map(_.map(f)))
//         case c @ LineCase(_, text) => LineCase(c.index, text)
//         case c @ ContentCase(_, _) => ContentCase(c.child.map(f), c.data)

//     object VFileCase:

//       final case class FileCase[-R, +E, +A](path: String, children: ZIO[R, E, Chunk[A]])
//           extends VFileCase[R, E, A, Nothing]
//       final case class LineCase(index: Int, text: String)
//           extends VFileCase[Any, Nothing, Nothing, Nothing]
//       final case class ContentCase[-R, +E, +A, +D](child: ZIO[R, E, A], data: D)
//           extends VFileCase[R, E, A, D]

//   end VFile

//   final case class VFileMessage(reason: VFileMessage.Reason)
//   object VFileMessage:
//     enum Reason:
//       case Description(text: String)
//       case Error(value: Throwable)
//   end VFileMessage

//   object LineNumber extends Newtype[Int]:
//     import zio.prelude.Assertion.*

//     extension (self: LineNumber) def ++ = wrap(unwrap(self) + 1)

//     val default: LineNumber       = wrap(1)
//     override inline def assertion = greaterThanOrEqualTo(1)

//   type LineNumber = LineNumber.Type

//   opaque type ColumnNumber = Int

//   final case class Offset(line: Option[LineNumber], column: Option[ColumnNumber])
//   object Offset:
//     def apply(line: LineNumber)                       = new Offset(Some(line), None)
//     def apply(line: LineNumber, column: ColumnNumber) = new Offset(Some(line), Some(column))
//     object Empty:
//       def apply: Offset = Offset(None, None)
//       def unapply(offset: Offset): Option[Unit] = offset match
//         case Offset(None, None) => Some(())
//         case _                  => None

//   final case class Position(start: Option[Offset], end: Option[Offset])
//   object Position:
//     def apply(start: Offset) = new Position(Some(start), None)
