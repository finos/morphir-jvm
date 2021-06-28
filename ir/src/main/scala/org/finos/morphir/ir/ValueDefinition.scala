package org.finos.morphir.ir

final case class ValueDefinition[+TA,+VA](
    inputTypes: List[ParameterInfo[TA,VA]], 
    outputType: Type[TA],
    body: Value[TA,VA])

final case class ParameterInfo[+TA,+VA](name:Name, annotations:VA, parameterType:Type[TA])