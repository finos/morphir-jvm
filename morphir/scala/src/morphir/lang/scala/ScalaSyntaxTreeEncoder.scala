package morphir.lang.scala

import morphir.ir.analyzer.Scope

import scala.meta.Tree

trait ScalaSyntaxTreeEncoder[A] {
  def treeFor[S](node: A)(implicit scope: Scope[S]): Tree
}

object ScalaSyntaxTreeEncoder {}
