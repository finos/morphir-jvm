package org.finos.morphir.knowledge

package object core {
  type FieldConstraint = PartialFunction[State, State]
  type Name            = String
  type Value           = Any
}
