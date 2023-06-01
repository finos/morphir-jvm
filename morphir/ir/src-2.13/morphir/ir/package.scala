package morphir.ir

import morphir.dependency.DAG
import morphir.ir.AccessControlled.AccessControlled
import morphir.ir.Module.{ModuleName, QualifiedModuleName}
import morphir.sdk.{Dict, Maybe, Result}
import morphir.sdk.Result.Ok

/** Generated based on IR.Package
*/
object Package{

  implicit def qualifiedModuleNameOrdering: Ordering[QualifiedModuleName] = (_: QualifiedModuleName, _: QualifiedModuleName) => 0
  implicit def moduleNameOrdering: Ordering[ModuleName] = (_: ModuleName, _: ModuleName) => 0

  final case class Definition[Ta, Va](
    modules: morphir.sdk.Dict.Dict[morphir.ir.Module.ModuleName, morphir.ir.AccessControlled.AccessControlled[morphir.ir.Module.Definition[Ta, Va]]]
  ){}
  
  type PackageName = morphir.ir.Path.Path
  
  final case class Specification[Ta](
    modules: morphir.sdk.Dict.Dict[morphir.ir.Module.ModuleName, morphir.ir.Module.Specification[Ta]]
  ){}
  
  def definitionToSpecification[Ta, Va](
    _def: morphir.ir.Package.Definition[Ta, Va]
  ): morphir.ir.Package.Specification[Ta] =
    morphir.ir.Package.Specification(modules = morphir.sdk.Dict.fromList(morphir.sdk.List.filterMap(({
      case (path, accessControlledModule) => 
        morphir.sdk.Maybe.map(((moduleDef: morphir.ir.Module.Definition[Ta, Va]) =>
          (path, morphir.ir.Module.definitionToSpecification(moduleDef))))(morphir.ir.AccessControlled.withPublicAccess(accessControlledModule))
    } : ((morphir.ir.Module.ModuleName, morphir.ir.AccessControlled.AccessControlled[morphir.ir.Module.Definition[Ta, Va]])) => morphir.sdk.Maybe.Maybe[(morphir.ir.Module.ModuleName, morphir.ir.Module.Specification[Ta])]))(morphir.sdk.Dict.toList(_def.modules))))
  
  def definitionToSpecificationWithPrivate[Ta, Va](
    _def: morphir.ir.Package.Definition[Ta, Va]
  ): morphir.ir.Package.Specification[Ta] =
    morphir.ir.Package.Specification(modules = morphir.sdk.Dict.fromList(morphir.sdk.List.map(({
      case (path, accessControlledModule) => 
        (path, morphir.ir.Module.definitionToSpecificationWithPrivate(morphir.ir.AccessControlled.withPrivateAccess(accessControlledModule)))
    } : ((morphir.ir.Module.ModuleName, morphir.ir.AccessControlled.AccessControlled[morphir.ir.Module.Definition[Ta, Va]])) => (morphir.ir.Module.ModuleName, morphir.ir.Module.Specification[Ta])))(morphir.sdk.Dict.toList(_def.modules))))
  
  def emptyDefinition[Ta, Va]: morphir.ir.Package.Definition[Ta, Va] =
    morphir.ir.Package.Definition(modules = morphir.sdk.Dict.empty)
  
  def emptySpecification[Ta]: morphir.ir.Package.Specification[Ta] =
    morphir.ir.Package.Specification(modules = morphir.sdk.Dict.empty)
  
  def eraseDefinitionAttributes[Ta, Va](
    _def: morphir.ir.Package.Definition[Ta, Va]
  ): morphir.ir.Package.Definition[scala.Unit, scala.Unit] =
    morphir.ir.Package.mapDefinitionAttributes(({
      case _ => 
        {}
    } : Ta => scala.Unit))(({
      case _ => 
        {}
    } : Va => scala.Unit))(_def)
  
  def eraseSpecificationAttributes[Ta](
    spec: morphir.ir.Package.Specification[Ta]
  ): morphir.ir.Package.Specification[scala.Unit] =
    morphir.ir.Package.mapSpecificationAttributes(({
      case _ => 
        {}
    } : Ta => scala.Unit))(spec)
  
  def lookupModuleDefinition[Ta, Va](
    modulePath: morphir.ir.Path.Path
  )(
    packageDef: morphir.ir.Package.Definition[Ta, Va]
  ): morphir.sdk.Maybe.Maybe[morphir.ir.Module.Definition[Ta, Va]] =
    morphir.sdk.Maybe.map(morphir.ir.AccessControlled.withPrivateAccess[morphir.ir.Module.Definition[Ta, Va]])(morphir.sdk.Dict.get(modulePath)(packageDef.modules))
  
  def lookupModuleSpecification[Ta](
    modulePath: morphir.ir.Path.Path
  )(
    packageSpec: morphir.ir.Package.Specification[Ta]
  ): morphir.sdk.Maybe.Maybe[morphir.ir.Module.Specification[Ta]] =
    morphir.sdk.Dict.get(modulePath)(packageSpec.modules)
  
  def lookupTypeSpecification[Ta](
    modulePath: morphir.ir.Path.Path
  )(
    localName: morphir.ir.Name.Name
  )(
    packageSpec: morphir.ir.Package.Specification[Ta]
  ): morphir.sdk.Maybe.Maybe[morphir.ir.Type.Specification[Ta]] =
    morphir.sdk.Maybe.andThen(morphir.ir.Module.lookupTypeSpecification[Ta](localName))(morphir.ir.Package.lookupModuleSpecification(modulePath)(packageSpec))
  
  def lookupValueDefinition[Ta, Va](
    modulePath: morphir.ir.Path.Path
  )(
    localName: morphir.ir.Name.Name
  )(
    packageDef: morphir.ir.Package.Definition[Ta, Va]
  ): morphir.sdk.Maybe.Maybe[morphir.ir.Value.Definition[Ta, Va]] =
    morphir.sdk.Maybe.andThen(morphir.ir.Module.lookupValueDefinition[Ta, Va](localName))(morphir.ir.Package.lookupModuleDefinition(modulePath)(packageDef))
  
  def lookupValueSpecification[Ta](
    modulePath: morphir.ir.Path.Path
  )(
    localName: morphir.ir.Name.Name
  )(
    packageSpec: morphir.ir.Package.Specification[Ta]
  ): morphir.sdk.Maybe.Maybe[morphir.ir.Value.Specification[Ta]] =
    morphir.sdk.Maybe.andThen(morphir.ir.Module.lookupValueSpecification[Ta](localName))(morphir.ir.Package.lookupModuleSpecification(modulePath)(packageSpec))
  
  def mapDefinitionAttributes[Ta, Tb, Va, Vb](
    tf: Ta => Tb
  )(
    vf: Va => Vb
  )(
    _def: morphir.ir.Package.Definition[Ta, Va]
  ): morphir.ir.Package.Definition[Tb, Vb] =
    (morphir.ir.Package.Definition(morphir.sdk.Dict.map(({
      case _ => 
        ((moduleDef: morphir.ir.AccessControlled.AccessControlled[morphir.ir.Module.Definition[Ta, Va]]) =>
          (morphir.ir.AccessControlled.AccessControlled(
            moduleDef.access,
            morphir.ir.Module.mapDefinitionAttributes(tf)(vf)(moduleDef.value)
          ) : morphir.ir.AccessControlled.AccessControlled[morphir.ir.Module.Definition[Tb, Vb]]))
    } : morphir.ir.Module.ModuleName => morphir.ir.AccessControlled.AccessControlled[morphir.ir.Module.Definition[Ta, Va]] => morphir.ir.AccessControlled.AccessControlled[morphir.ir.Module.Definition[Tb, Vb]]))(_def.modules)) : morphir.ir.Package.Definition[Tb, Vb])
  
  def mapSpecificationAttributes[Ta, Tb](
    tf: Ta => Tb
  )(
    spec: morphir.ir.Package.Specification[Ta]
  ): morphir.ir.Package.Specification[Tb] =
    (morphir.ir.Package.Specification(morphir.sdk.Dict.map(({
      case _ => 
        ((moduleSpec: morphir.ir.Module.Specification[Ta]) =>
          morphir.ir.Module.mapSpecificationAttributes(tf)(moduleSpec))
    } : morphir.ir.Module.ModuleName => morphir.ir.Module.Specification[Ta] => morphir.ir.Module.Specification[Tb]))(spec.modules)) : morphir.ir.Package.Specification[Tb])
  
  def modulesOrderedByDependency(
    packageName: morphir.ir.Package.PackageName
  )(
    packageDef: morphir.ir.Package.Definition[scala.Unit, morphir.ir.Type.Type[scala.Unit]]
  ): morphir.sdk.Result.Result[morphir.dependency.DAG.CycleDetected[morphir.ir.Module.ModuleName], morphir.sdk.List.List[(morphir.ir.Module.ModuleName, morphir.ir.AccessControlled.AccessControlled[morphir.ir.Module.Definition[scala.Unit, morphir.ir.Type.Type[scala.Unit]]])]] = {
    import scala.util.chaining._
    import morphir.ir.Type.Type
    import morphir.sdk.List

    packageDef.modules
      .pipe(Dict.toList[ModuleName, AccessControlled[Module.Definition[Unit, Type[Unit]]]])
      .pipe(List.foldl((pair: (ModuleName, AccessControlled[Module.Definition[Unit, Type[Unit]]])) => (dagResultSoFar: Result.Result[DAG.CycleDetected[ModuleName], DAG.DAG[ModuleName]]) => {
        val (moduleName, accessControlledModuleDef) = pair
        val dependsOnModules: Set[ModuleName] =
          accessControlledModuleDef.value
            .pipe(Module.dependsOnModules)
            .pipe(morphir.sdk.Set.filter { case (dependsOnPackage, _) => dependsOnPackage == packageName })
            .pipe(morphir.sdk.Set.map { case (_, second) => second })

        dagResultSoFar
          .pipe(Result.andThen(DAG.insertNode(moduleName)(dependsOnModules)))
      })(Ok(DAG.empty[ModuleName])))
      .pipe(Result.map {
        moduleDependencies =>
          moduleDependencies
            .pipe(DAG.backwardTopologicalOrdering)
            .pipe(List.concat[ModuleName])
            .pipe(List.filterMap {
              moduleName: ModuleName =>
                packageDef.modules
                  .pipe(Dict.get(moduleName))
                  .pipe(Maybe.map(v => (moduleName, v)))
            })
      })
  }
  
  def selectModules[Ta, Va](
    modulesToInclude: morphir.sdk.Set.Set[morphir.ir.Module.ModuleName]
  )(
    packageName: morphir.ir.Package.PackageName
  )(
    packageDef: morphir.ir.Package.Definition[Ta, Va]
  ): morphir.ir.Package.Definition[Ta, Va] = {
    def findAllDependencies(
      current: morphir.sdk.Set.Set[morphir.ir.Module.ModuleName]
    ): morphir.sdk.Set.Set[morphir.ir.Module.ModuleName] =
      morphir.sdk.List.foldl(morphir.sdk.Set.union[ModuleName])(morphir.sdk.Set.empty)(morphir.sdk.List.filterMap(((currentModuleName: morphir.ir.Module.ModuleName) =>
        morphir.sdk.Maybe.map(((mDef: morphir.ir.AccessControlled.AccessControlled[morphir.ir.Module.Definition[Ta, Va]]) =>
          morphir.sdk.Set.fromList(morphir.sdk.List.filterMap(({
            case (pName, mName) => 
              if (morphir.sdk.Basics.equal(pName)(packageName)) {
                (morphir.sdk.Maybe.Just(mName) : morphir.sdk.Maybe.Maybe[morphir.ir.Path.Path])
              } else {
                (morphir.sdk.Maybe.Nothing : morphir.sdk.Maybe.Maybe[morphir.ir.Path.Path])
              }
          } : ((morphir.ir.Path.Path, morphir.ir.Path.Path)) => morphir.sdk.Maybe.Maybe[morphir.ir.Path.Path]))(morphir.sdk.Set.toList(morphir.ir.Module.dependsOnModules(mDef.value))))))(morphir.sdk.Dict.get(currentModuleName)(packageDef.modules))))(morphir.sdk.Set.toList(current)))
    
    val expandedModulesToInclude: morphir.sdk.Set.Set[morphir.ir.Module.ModuleName] = morphir.sdk.Set.union(findAllDependencies(modulesToInclude))(modulesToInclude)
    
    if (morphir.sdk.Basics.equal(modulesToInclude)(expandedModulesToInclude)) {
      packageDef.copy(modules = morphir.sdk.Dict.fromList(morphir.sdk.List.filter(({
        case (moduleName, _) => 
          morphir.sdk.Set.member(moduleName)(modulesToInclude)
      } : ((morphir.ir.Module.ModuleName, morphir.ir.AccessControlled.AccessControlled[morphir.ir.Module.Definition[Ta, Va]])) => morphir.sdk.Basics.Bool))(morphir.sdk.Dict.toList(packageDef.modules))))
    } else {
      morphir.ir.Package.selectModules(expandedModulesToInclude)(packageName)(packageDef)
    }
  }

}