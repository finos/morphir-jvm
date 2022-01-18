package zio.morphir.ir

final case class Attributed[Case[+_], A](caseValue: Case[Attributed[Case, A]], attributes: A)
