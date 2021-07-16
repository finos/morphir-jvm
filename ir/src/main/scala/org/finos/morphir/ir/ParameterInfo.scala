package org.finos.morphir.ir

final case class ParameterInfo[+TA, +VA](
    name: Name,
    annotations: VA,
    parameterType: Type[TA]
  )
