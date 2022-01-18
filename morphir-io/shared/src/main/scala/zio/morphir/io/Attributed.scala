package zio.morphir.vfile

import zio.*

final case class Attributed[Case[+_], +A](
    caseValue: Case[Attributed[Case, A]],
    attributes: ZEnvironment[A]
)
