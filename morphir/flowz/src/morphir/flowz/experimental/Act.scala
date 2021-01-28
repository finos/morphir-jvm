package morphir.flowz.experimental

import zio._

import context.Context
import morphir.flowz.StepOutputs

sealed abstract class Act[-StateIn, +StateOut, -Env, -Params, +Err, +Value] extends Product with Serializable {
  def effect: ZIO[Env with StateIn with Params with Context, Err, StepOutputs[StateOut, Value]]

}

object Act {
  //final case class FromFuncAlgebra[StateIn, Params, StateOut, Value](func:(StateIn,Params) => )
  final case class FromFunc[In, Out](func: In => Out) {}
}

object input {
  type Input[A] = Has[Input.Data[A]]

  def value[A: Tag]: URIO[Input[A], A] = ZIO.access(_.get.value)

  object Input {
    final case class Data[A](value: A) {}
  }
}

object context {
  type Context = Has[Context.Service]
  object Context {
    trait Service {}
  }
}
