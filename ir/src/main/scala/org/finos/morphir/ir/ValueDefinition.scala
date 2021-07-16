package org.finos.morphir.ir

/**
 * Type that represents a value or function definition. A definition is the actual data or logic as
 * opposed to a specification which is just the specification of those. Value definitions can be
 * typed or untyped. Exposed values have to be typed.
 */
final case class ValueDefinition[+TA, +VA](
    inputTypes: List[ParameterInfo[TA, VA]],
    outputType: Type[TA],
    body: Value[TA, VA]
  )
