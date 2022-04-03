package zio.morphir.ir.module

trait ModuleSpecFor[A] {

  def module: ModuleName
  def spec: Specification[Any]
}

object ModuleSpecFor {

  /** Summon the module specification for the given module/type. */
  def apply[A](implicit specFor: ModuleSpecFor[A]): ModuleSpecFor[A] = specFor

  def make[A](name: ModuleName)(moduleSpec: Specification[Any]): ModuleSpecFor[A] =
    new ModuleSpecFor[A] {
      val module = name
      val spec   = moduleSpec
    }
}
