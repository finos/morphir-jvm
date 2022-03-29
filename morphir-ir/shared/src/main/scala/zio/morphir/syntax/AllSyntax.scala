package zio.morphir.syntax

import zio.morphir.ir.types.TypeSyntax

trait AllSyntax extends NamingSyntax with TypeSyntax with ValueSyntax

object all extends AllSyntax
