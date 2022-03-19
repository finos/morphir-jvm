package zio.morphir.syntax

trait AllSyntax extends NamingSyntax with TypeSyntax with ValueSyntax

object all extends AllSyntax
