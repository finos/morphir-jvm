package zio.morphir.ir.module
import zio.morphir.ir.{Name, Value}

trait ModuleModule {

  final type Definition[+TA, +VA] = zio.morphir.ir.module.Definition[TA, VA]
  final val Definition: zio.morphir.ir.module.Definition.type = zio.morphir.ir.module.Definition

  final type ModuleName = zio.morphir.ir.module.ModuleName
  final val ModuleName: zio.morphir.ir.module.ModuleName.type = zio.morphir.ir.module.ModuleName

  final type ModulePath = zio.morphir.ir.module.ModulePath
  final val ModulePath: zio.morphir.ir.module.ModulePath.type = zio.morphir.ir.module.ModulePath

  final type QualifiedModuleName = zio.morphir.ir.module.QualifiedModuleName
  final val QualifiedModuleName: zio.morphir.ir.module.QualifiedModuleName.type =
    zio.morphir.ir.module.QualifiedModuleName

  final type Specification[+TA] = zio.morphir.ir.module.Specification[TA]
  final val Specification: zio.morphir.ir.module.Specification.type = zio.morphir.ir.module.Specification

  final type USpecification = zio.morphir.ir.module.Specification[Any]
  final val USpecification: zio.morphir.ir.module.Specification.type = zio.morphir.ir.module.Specification

  final val emptyDefinition: Definition[Nothing, Nothing] = Definition.empty
  final val emptySpecification: Specification[Nothing]    = Specification.empty

  final def lookupValueDefinition[TA, VA](
      localName: Name,
      moduleDef: Definition[TA, VA]
  ): Option[Value.Definition[TA, VA]] =
    moduleDef.lookupValueDefinition(localName)
}

object ModuleModule extends ModuleModule
