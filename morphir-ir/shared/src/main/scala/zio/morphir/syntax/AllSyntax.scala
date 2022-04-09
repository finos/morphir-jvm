package zio.morphir.syntax

import zio.morphir.ir.types.nonrecursive.{FieldSyntax, TypeSyntax}
import zio.morphir.ir.value.ValueSyntax

trait AllSyntax extends NamingSyntax with FieldSyntax with TypeSyntax with ValueSyntax

object all extends AllSyntax
