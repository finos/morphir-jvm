package zio.morphir.syntax

import zio.morphir.ir.types.TypeSyntax
import zio.morphir.ir.value.ValueSyntax

trait AllSyntax extends NamingSyntax with TypeSyntax with ValueSyntax

object all extends AllSyntax
