package morphir.ir

package object advanced {
  type UpdateFieldsList[X] = List[(Name, Value[X])]
  type PatternMatchCasesList[X] = List[(Pattern[X], Value[X])]

}
