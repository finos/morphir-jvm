package morphir.ir

/**
 * Generated based on IR.Distribution
 */
object Distribution {

  sealed trait Distribution {}

  object Distribution {

    final case class Library(
      arg1: morphir.ir.Package.PackageName,
      arg2: morphir.sdk.Dict.Dict[morphir.ir.Package.PackageName, morphir.ir.Package.Specification[scala.Unit]],
      arg3: morphir.ir.Package.Definition[scala.Unit, morphir.ir.Type.Type[scala.Unit]]
    ) extends morphir.ir.Distribution.Distribution {}

  }

  val Library: morphir.ir.Distribution.Distribution.Library.type = morphir.ir.Distribution.Distribution.Library

  def insertDependency(
    dependencyPackageName: morphir.ir.Package.PackageName
  )(
    dependencyPackageSpec: morphir.ir.Package.Specification[scala.Unit]
  )(
    distribution: morphir.ir.Distribution.Distribution
  ): morphir.ir.Distribution.Distribution =
    distribution match {
      case morphir.ir.Distribution.Library(packageName, dependencies, packageDef) =>
        (morphir.ir.Distribution.Library(
          packageName,
          morphir.sdk.Dict.insert(dependencyPackageName)(dependencyPackageSpec)(dependencies),
          packageDef
        ): morphir.ir.Distribution.Distribution)
    }

  def lookupBaseTypeName: morphir.ir.FQName.FQName => morphir.ir.Distribution.Distribution => morphir.sdk.Maybe.Maybe[
    morphir.ir.FQName.FQName
  ] =
    ({ case fQName @ (packageName, moduleName, localName) =>
      (
        (distribution: morphir.ir.Distribution.Distribution) =>
          morphir.sdk.Maybe.andThen(
            (
              (typeSpec: morphir.ir.Type.Specification[scala.Unit]) =>
                typeSpec match {
                  case morphir.ir.Type.TypeAliasSpecification(_, morphir.ir.Type.Reference(_, aliasFQName, _)) =>
                    morphir.ir.Distribution.lookupBaseTypeName(aliasFQName)(distribution)
                  case _ =>
                    (morphir.sdk.Maybe.Just(fQName): morphir.sdk.Maybe.Maybe[
                      (morphir.ir.Path.Path, morphir.ir.Path.Path, morphir.ir.Name.Name)
                    ])
                }
            )
          )(
            morphir.sdk.Maybe.andThen(morphir.ir.Module.lookupTypeSpecification[Unit](localName))(
              morphir.ir.Distribution.lookupModuleSpecification(packageName)(moduleName)(distribution)
            )
          )
      )
    }: morphir.ir.FQName.FQName => morphir.ir.Distribution.Distribution => morphir.sdk.Maybe.Maybe[
      morphir.ir.FQName.FQName
    ])

  def lookupModuleSpecification(
    packageName: morphir.ir.Package.PackageName
  )(
    modulePath: morphir.ir.Module.ModuleName
  )(
    distribution: morphir.ir.Distribution.Distribution
  ): morphir.sdk.Maybe.Maybe[morphir.ir.Module.Specification[scala.Unit]] =
    distribution match {
      case morphir.ir.Distribution.Library(libraryPackageName, dependencies, packageDef) =>
        if (morphir.sdk.Basics.equal(packageName)(libraryPackageName)) {
          morphir.ir.Package.lookupModuleSpecification(modulePath)(
            morphir.ir.Package.definitionToSpecification(packageDef)
          )
        } else {
          morphir.sdk.Maybe.andThen(morphir.ir.Package.lookupModuleSpecification[Unit](modulePath))(
            morphir.sdk.Dict.get(packageName)(dependencies)
          )
        }
    }

  def lookupPackageName(
    distribution: morphir.ir.Distribution.Distribution
  ): morphir.ir.Package.PackageName =
    distribution match {
      case morphir.ir.Distribution.Library(packageName, _, _) =>
        packageName
    }

  def lookupPackageSpecification(
    distribution: morphir.ir.Distribution.Distribution
  ): morphir.ir.Package.Specification[scala.Unit] =
    distribution match {
      case morphir.ir.Distribution.Library(_, _, packageDef) =>
        morphir.ir.Package.mapSpecificationAttributes(({
          case _ => {}
        }: scala.Unit => scala.Unit))(morphir.ir.Package.definitionToSpecificationWithPrivate(packageDef))
    }

  def lookupTypeSpecification(
    packageName: morphir.ir.Package.PackageName
  )(
    moduleName: morphir.ir.Module.ModuleName
  )(
    localName: morphir.ir.Name.Name
  )(
    distribution: morphir.ir.Distribution.Distribution
  ): morphir.sdk.Maybe.Maybe[morphir.ir.Type.Specification[scala.Unit]] =
    morphir.sdk.Maybe.andThen(morphir.ir.Module.lookupTypeSpecification[Unit](localName))(
      morphir.ir.Distribution.lookupModuleSpecification(packageName)(moduleName)(distribution)
    )

  def lookupValueDefinition: morphir.ir.QName.QName => morphir.ir.Distribution.Distribution => morphir.sdk.Maybe.Maybe[
    morphir.ir.Value.Definition[scala.Unit, morphir.ir.Type.Type[scala.Unit]]
  ] =
    ({ case morphir.ir.QName.QName(moduleName, localName) =>
      ({ case morphir.ir.Distribution.Library(_, _, packageDef) =>
        morphir.sdk.Maybe.andThen(morphir.ir.Module.lookupValueDefinition[Unit, morphir.ir.Type.Type[Unit]](localName))(
          morphir.ir.Package.lookupModuleDefinition(moduleName)(packageDef)
        )
      })
    }: morphir.ir.QName.QName => morphir.ir.Distribution.Distribution => morphir.sdk.Maybe.Maybe[
      morphir.ir.Value.Definition[scala.Unit, morphir.ir.Type.Type[scala.Unit]]
    ])

  def lookupValueSpecification(
    packageName: morphir.ir.Package.PackageName
  )(
    moduleName: morphir.ir.Module.ModuleName
  )(
    localName: morphir.ir.Name.Name
  )(
    distribution: morphir.ir.Distribution.Distribution
  ): morphir.sdk.Maybe.Maybe[morphir.ir.Value.Specification[scala.Unit]] =
    morphir.sdk.Maybe.andThen(morphir.ir.Module.lookupValueSpecification[Unit](localName))(
      morphir.ir.Distribution.lookupModuleSpecification(packageName)(moduleName)(distribution)
    )

}
