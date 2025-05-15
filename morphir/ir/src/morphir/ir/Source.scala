package morphir.ir

/** Generated based on IR.Source
*/
object Source{

  implicit def moduleNameOrdering: Ordering[DataSourceName] = (_: DataSourceName, _: DataSourceName) => 0

  final case class Component(
    name: morphir.ir.Source.ComponentName,
    inputs: morphir.sdk.Dict.Dict[morphir.ir.Source.DataSourceName, morphir.ir.Source.DataType],
    states: morphir.sdk.Dict.Dict[morphir.ir.Source.DataSourceName, morphir.ir.Source.DataType],
    outputs: morphir.sdk.Dict.Dict[morphir.ir.Source.OutputName, morphir.sdk.List.List[morphir.ir.Source.OutputSource]]
  ){}
  
  type ComponentName = morphir.ir.Path.Path
  
  type DataSourceName = morphir.ir.Name.Name
  
  sealed trait DataType {
  
    
  
  }
  
  object DataType{
  
    final case class Literal(
      arg1: morphir.ir.Source.LiteralType
    ) extends morphir.ir.Source.DataType{}
    
    final case class RowSet(
      arg1: morphir.ir.FQName.FQName
    ) extends morphir.ir.Source.DataType{}
  
  }
  
  val Literal: morphir.ir.Source.DataType.Literal.type  = morphir.ir.Source.DataType.Literal
  
  val RowSet: morphir.ir.Source.DataType.RowSet.type  = morphir.ir.Source.DataType.RowSet
  
  sealed trait Error {
  
    
  
  }
  
  object Error{
  
    final case class BrokenFunctionReference(
      arg1: morphir.ir.FQName.FQName
    ) extends morphir.ir.Source.Error{}
    
    final case class CyclicDependency(
      arg1: morphir.dependency.DAG.CycleDetected[(morphir.ir.Source.NodeType, morphir.ir.FQName.FQName)]
    ) extends morphir.ir.Source.Error{}
    
    final case class InputStateNameConflict(
      arg1: morphir.sdk.List.List[morphir.ir.Source.DataSourceName]
    ) extends morphir.ir.Source.Error{}
    
    final case class MissingPackage(
      arg1: morphir.ir.Package.PackageName
    ) extends morphir.ir.Source.Error{}
    
    final case class MultiplePackageShareSameName(
      arg1: morphir.sdk.Basics.Int,
      arg2: morphir.ir.Package.PackageName
    ) extends morphir.ir.Source.Error{}
    
    final case class OutputSourceTypeMismatch(
      arg1: morphir.ir.Source.OutputName,
      arg2: morphir.ir.Type.Type[scala.Unit],
      arg3: morphir.ir.Type.Type[scala.Unit]
    ) extends morphir.ir.Source.Error{}
    
    final case class ParamNotSupplied(
      arg1: morphir.ir.Source.OutputName,
      arg2: morphir.sdk.Basics.Int,
      arg3: morphir.ir.FQName.FQName,
      arg4: morphir.ir.Source.ParameterName
    ) extends morphir.ir.Source.Error{}
    
    final case class UnknownInputStateReference(
      arg1: morphir.ir.Source.OutputName,
      arg2: morphir.sdk.List.List[morphir.ir.Source.DataSourceName]
    ) extends morphir.ir.Source.Error{}
    
    final case class UnusedInputOrState(
      arg1: morphir.ir.Source.DataSourceName
    ) extends morphir.ir.Source.Error{}
  
  }
  
  val BrokenFunctionReference: morphir.ir.Source.Error.BrokenFunctionReference.type  = morphir.ir.Source.Error.BrokenFunctionReference
  
  val CyclicDependency: morphir.ir.Source.Error.CyclicDependency.type  = morphir.ir.Source.Error.CyclicDependency
  
  val InputStateNameConflict: morphir.ir.Source.Error.InputStateNameConflict.type  = morphir.ir.Source.Error.InputStateNameConflict
  
  val MissingPackage: morphir.ir.Source.Error.MissingPackage.type  = morphir.ir.Source.Error.MissingPackage
  
  val MultiplePackageShareSameName: morphir.ir.Source.Error.MultiplePackageShareSameName.type  = morphir.ir.Source.Error.MultiplePackageShareSameName
  
  val OutputSourceTypeMismatch: morphir.ir.Source.Error.OutputSourceTypeMismatch.type  = morphir.ir.Source.Error.OutputSourceTypeMismatch
  
  val ParamNotSupplied: morphir.ir.Source.Error.ParamNotSupplied.type  = morphir.ir.Source.Error.ParamNotSupplied
  
  val UnknownInputStateReference: morphir.ir.Source.Error.UnknownInputStateReference.type  = morphir.ir.Source.Error.UnknownInputStateReference
  
  val UnusedInputOrState: morphir.ir.Source.Error.UnusedInputOrState.type  = morphir.ir.Source.Error.UnusedInputOrState
  
  sealed trait LiteralType {
  
    
  
  }
  
  object LiteralType{
  
    case object BoolLiteral extends morphir.ir.Source.LiteralType{}
    
    case object DecimalLiteral extends morphir.ir.Source.LiteralType{}
    
    case object FloatLiteral extends morphir.ir.Source.LiteralType{}
    
    case object LocalDateLiteral extends morphir.ir.Source.LiteralType{}
    
    case object LocalTimeLiteral extends morphir.ir.Source.LiteralType{}
    
    case object StringLiteral extends morphir.ir.Source.LiteralType{}
    
    case object WholeNumberLiteral extends morphir.ir.Source.LiteralType{}
  
  }
  
  val BoolLiteral: morphir.ir.Source.LiteralType.BoolLiteral.type  = morphir.ir.Source.LiteralType.BoolLiteral
  
  val DecimalLiteral: morphir.ir.Source.LiteralType.DecimalLiteral.type  = morphir.ir.Source.LiteralType.DecimalLiteral
  
  val FloatLiteral: morphir.ir.Source.LiteralType.FloatLiteral.type  = morphir.ir.Source.LiteralType.FloatLiteral
  
  val LocalDateLiteral: morphir.ir.Source.LiteralType.LocalDateLiteral.type  = morphir.ir.Source.LiteralType.LocalDateLiteral
  
  val LocalTimeLiteral: morphir.ir.Source.LiteralType.LocalTimeLiteral.type  = morphir.ir.Source.LiteralType.LocalTimeLiteral
  
  val StringLiteral: morphir.ir.Source.LiteralType.StringLiteral.type  = morphir.ir.Source.LiteralType.StringLiteral
  
  val WholeNumberLiteral: morphir.ir.Source.LiteralType.WholeNumberLiteral.type  = morphir.ir.Source.LiteralType.WholeNumberLiteral
  
  type NodeType = morphir.sdk.String.String
  
  type OutputName = morphir.ir.Name.Name
  
  final case class OutputSource(
    functionReference: morphir.ir.FQName.FQName,
    arguments: morphir.sdk.Dict.Dict[morphir.ir.Source.ParameterName, morphir.ir.Source.DataSourceName]
  ){}
  
  type ParameterName = morphir.ir.Name.Name
  
  def collectInputAndStateNameConflictErrors(
    comp: morphir.ir.Source.Component
  ): morphir.sdk.List.List[morphir.ir.Source.Error] =
    morphir.sdk.Set.toList(morphir.sdk.Set.intersect(morphir.sdk.Set.fromList(morphir.sdk.Dict.keys(comp.states)))(morphir.sdk.Set.fromList(morphir.sdk.Dict.keys(comp.inputs)))) match {
      case Nil => 
        morphir.sdk.List(
        
        )
      case conflicts => 
        morphir.sdk.List.cons((morphir.ir.Source.InputStateNameConflict(conflicts) : morphir.ir.Source.Error))(morphir.sdk.List(
        
        ))
    }
  
  def collectNonInputReferenceErrors(
    comp: morphir.ir.Source.Component
  ): morphir.sdk.List.List[morphir.ir.Source.Error] = {
    val inputsAndStates: morphir.sdk.Set.Set[morphir.ir.Source.DataSourceName] = morphir.sdk.Set.fromList(morphir.sdk.List.append(morphir.sdk.Dict.keys(comp.states))(morphir.sdk.Dict.keys(comp.inputs)))
    
    morphir.sdk.Dict.foldl(((outputName: morphir.ir.Name.Name) =>
      ((outputSources: morphir.sdk.List.List[morphir.ir.Source.OutputSource]) =>
        ((acc: morphir.sdk.List.List[morphir.ir.Source.Error]) =>
          morphir.sdk.List.filter(((argName: morphir.ir.Source.DataSourceName) =>
            morphir.sdk.Basics.not(morphir.sdk.Set.member(argName)(inputsAndStates))))(morphir.sdk.List.concat(morphir.sdk.List.map(morphir.sdk.Basics.composeRight(((x: morphir.ir.Source.OutputSource) =>
            x.arguments))(morphir.sdk.Dict.values))(outputSources))) match {
            case Nil => 
              acc
            case unknownArgs => 
              morphir.sdk.List.cons((morphir.ir.Source.UnknownInputStateReference(
                outputName,
                unknownArgs
              ) : morphir.ir.Source.Error))(acc)
          }))))(morphir.sdk.List(
    
    ))(comp.outputs)
  }
  
  def collectNonUniqueDistributionErrors(
    distros: morphir.sdk.List.List[morphir.ir.Distribution.Distribution]
  ): morphir.sdk.List.List[morphir.ir.Source.Error] =
    morphir.sdk.List.map(({
      case (packageName, count) => 
        (morphir.ir.Source.MultiplePackageShareSameName(
          count,
          packageName
        ) : morphir.ir.Source.Error)
    } : ((morphir.ir.Package.PackageName, morphir.sdk.Basics.Int)) => morphir.ir.Source.Error))(morphir.sdk.Dict.toList(morphir.sdk.Dict.filter(({
      case _ => 
        ((count: morphir.sdk.Basics.Int) =>
          morphir.sdk.Basics.greaterThan(count)(morphir.sdk.Basics.Int(1)))
    } : morphir.ir.Package.PackageName => morphir.sdk.Basics.Int => morphir.sdk.Basics.Bool))(morphir.sdk.List.foldl(((distro: morphir.ir.Distribution.Distribution) =>
      ((counts: morphir.sdk.Dict.Dict[morphir.ir.Path.Path, morphir.sdk.Basics.Int]) =>
        distro match {
          case morphir.ir.Distribution.Library(packageName, _, _) => 
            morphir.sdk.Dict.update(packageName)(((v: morphir.sdk.Maybe.Maybe[morphir.sdk.Basics.Int]) =>
              v match {
                case morphir.sdk.Maybe.Just(value) => 
                  (morphir.sdk.Maybe.Just(morphir.sdk.Basics.add(value)(morphir.sdk.Basics.Int(1))) : morphir.sdk.Maybe.Maybe[morphir.sdk.Basics.Int])
                case morphir.sdk.Maybe.Nothing => 
                  (morphir.sdk.Maybe.Just(morphir.sdk.Basics.Int(1)) : morphir.sdk.Maybe.Maybe[morphir.sdk.Basics.Int])
              }))(counts)
        })))(morphir.sdk.Dict.empty)(distros))))
  
  def collectOutputSliceTypeMismatchErrors(
    comp: morphir.ir.Source.Component
  )(
    distros: morphir.sdk.List.List[morphir.ir.Distribution.Distribution]
  ): morphir.sdk.List.List[morphir.ir.Source.Error] = {
    val distroMaps = morphir.ir.Source.distrosByName(distros)
    
    morphir.sdk.Dict.foldl(((outputName: morphir.ir.Source.OutputName) =>
      ((outputSources: morphir.sdk.List.List[morphir.ir.Source.OutputSource]) =>
        ((acc: morphir.sdk.List.List[morphir.ir.Source.Error]) =>
          outputSources match {
            case Nil => 
              acc
            case (head :: rest) => 
              morphir.sdk.Maybe.map(((x: morphir.ir.Value.Definition[scala.Unit, morphir.ir.Type.Type[scala.Unit]]) =>
                x.outputType))(morphir.sdk.Maybe.andThen(morphir.ir.Distribution.lookupValueDefinition(head.functionReference))(morphir.sdk.Dict.get(morphir.ir.FQName.getPackagePath(head.functionReference))(distroMaps))) match {
                case morphir.sdk.Maybe.Just(outputType) => 
                  morphir.sdk.List.foldl(((source: morphir.ir.Source.OutputSource) =>
                    ((acc2: morphir.sdk.List.List[morphir.ir.Source.Error]) =>
                      morphir.sdk.Maybe.map(((x: morphir.ir.Value.Definition[scala.Unit, morphir.ir.Type.Type[scala.Unit]]) =>
                        x.outputType))(morphir.sdk.Maybe.andThen(morphir.ir.Distribution.lookupValueDefinition(source.functionReference))(morphir.sdk.Dict.get(morphir.ir.FQName.getPackagePath(source.functionReference))(distroMaps))) match {
                        case morphir.sdk.Maybe.Just(nextRefType) => 
                          if (morphir.sdk.Basics.equal(nextRefType)(outputType)) {
                            acc2
                          } else {
                            morphir.sdk.List.cons((morphir.ir.Source.OutputSourceTypeMismatch(
                              outputName,
                              outputType,
                              nextRefType
                            ) : morphir.ir.Source.Error))(acc2)
                          }
                        case morphir.sdk.Maybe.Nothing => 
                          morphir.sdk.List.cons((morphir.ir.Source.BrokenFunctionReference(source.functionReference) : morphir.ir.Source.Error))(acc2)
                      })))(acc)(rest)
                case morphir.sdk.Maybe.Nothing => 
                  morphir.sdk.List.cons((morphir.ir.Source.BrokenFunctionReference(head.functionReference) : morphir.ir.Source.Error))(acc)
              }
          }))))(morphir.sdk.List(
    
    ))(comp.outputs)
  }
  
  def collectUnusedInputStateErrors(
    comp: morphir.ir.Source.Component
  ): morphir.sdk.List.List[morphir.ir.Source.Error] = {
    val inputsAndStates: morphir.sdk.Set.Set[morphir.ir.Source.DataSourceName] = morphir.sdk.Set.fromList(morphir.sdk.List.append(morphir.sdk.Dict.keys(comp.states))(morphir.sdk.Dict.keys(comp.inputs)))
    
    val usedInputsAndStates: morphir.sdk.Set.Set[morphir.ir.Source.DataSourceName] = morphir.sdk.Set.fromList(morphir.sdk.List.concatMap(morphir.sdk.Basics.composeRight(((x: morphir.ir.Source.OutputSource) =>
      x.arguments))(morphir.sdk.Dict.values))(morphir.sdk.List.concat(morphir.sdk.Dict.values(comp.outputs))))
    
    morphir.sdk.List.map((morphir.ir.Source.UnusedInputOrState.apply : morphir.ir.Source.DataSourceName => morphir.ir.Source.Error))(morphir.sdk.Set.toList(morphir.sdk.Set.diff(inputsAndStates)(usedInputsAndStates)))
  }
  
  def component(
    name: morphir.ir.Source.ComponentName
  )(
    inputs: morphir.sdk.Dict.Dict[morphir.ir.Source.DataSourceName, morphir.ir.Source.DataType]
  )(
    states: morphir.sdk.Dict.Dict[morphir.ir.Source.DataSourceName, morphir.ir.Source.DataType]
  )(
    outputs: morphir.sdk.Dict.Dict[morphir.ir.Source.OutputName, morphir.sdk.List.List[morphir.ir.Source.OutputSource]]
  ): morphir.ir.Source.Component =
    morphir.ir.Source.Component(
      inputs = inputs,
      name = name,
      outputs = outputs,
      states = states
    )
  
  def dataTypeToType(
    dataType: morphir.ir.Source.DataType
  ): morphir.ir.Type.Type[scala.Unit] =
    dataType match {
      case morphir.ir.Source.RowSet(fQName) => 
        (morphir.ir.Type.Reference(
          {},
          (morphir.sdk.List(
            morphir.sdk.List("""morphir"""),
            morphir.sdk.List(
              """s""",
              """d""",
              """k"""
            )
          ), morphir.sdk.List(morphir.sdk.List("""list""")), morphir.sdk.List("""list""")),
          morphir.sdk.List((morphir.ir.Type.Reference(
            {},
            fQName,
            morphir.sdk.List(
            
            )
          ) : morphir.ir.Type.Type[scala.Unit]))
        ) : morphir.ir.Type.Type[scala.Unit])
      case morphir.ir.Source.Literal(literal) => 
        literal match {
          case morphir.ir.Source.BoolLiteral => 
            (morphir.ir.Type.Reference(
              {},
              (morphir.sdk.List(
                morphir.sdk.List("""morphir"""),
                morphir.sdk.List(
                  """s""",
                  """d""",
                  """k"""
                )
              ), morphir.sdk.List(morphir.sdk.List("""basics""")), morphir.sdk.List("""bool""")),
              morphir.sdk.List(
              
              )
            ) : morphir.ir.Type.Type[scala.Unit])
          case morphir.ir.Source.StringLiteral => 
            (morphir.ir.Type.Reference(
              {},
              (morphir.sdk.List(
                morphir.sdk.List("""morphir"""),
                morphir.sdk.List(
                  """s""",
                  """d""",
                  """k"""
                )
              ), morphir.sdk.List(morphir.sdk.List("""basics""")), morphir.sdk.List("""string""")),
              morphir.sdk.List(
              
              )
            ) : morphir.ir.Type.Type[scala.Unit])
          case morphir.ir.Source.WholeNumberLiteral => 
            (morphir.ir.Type.Reference(
              {},
              (morphir.sdk.List(
                morphir.sdk.List("""morphir"""),
                morphir.sdk.List(
                  """s""",
                  """d""",
                  """k"""
                )
              ), morphir.sdk.List(morphir.sdk.List("""basics""")), morphir.sdk.List("""int""")),
              morphir.sdk.List(
              
              )
            ) : morphir.ir.Type.Type[scala.Unit])
          case morphir.ir.Source.FloatLiteral => 
            (morphir.ir.Type.Reference(
              {},
              (morphir.sdk.List(
                morphir.sdk.List("""morphir"""),
                morphir.sdk.List(
                  """s""",
                  """d""",
                  """k"""
                )
              ), morphir.sdk.List(morphir.sdk.List("""basics""")), morphir.sdk.List("""float""")),
              morphir.sdk.List(
              
              )
            ) : morphir.ir.Type.Type[scala.Unit])
          case morphir.ir.Source.DecimalLiteral => 
            (morphir.ir.Type.Reference(
              {},
              (morphir.sdk.List(
                morphir.sdk.List("""morphir"""),
                morphir.sdk.List(
                  """s""",
                  """d""",
                  """k"""
                )
              ), morphir.sdk.List(morphir.sdk.List("""decimal""")), morphir.sdk.List("""decimal""")),
              morphir.sdk.List(
              
              )
            ) : morphir.ir.Type.Type[scala.Unit])
          case morphir.ir.Source.LocalDateLiteral => 
            (morphir.ir.Type.Reference(
              {},
              (morphir.sdk.List(
                morphir.sdk.List("""morphir"""),
                morphir.sdk.List(
                  """s""",
                  """d""",
                  """k"""
                )
              ), morphir.sdk.List(morphir.sdk.List(
                """local""",
                """date"""
              )), morphir.sdk.List(
                """local""",
                """date"""
              )),
              morphir.sdk.List(
              
              )
            ) : morphir.ir.Type.Type[scala.Unit])
          case morphir.ir.Source.LocalTimeLiteral => 
            (morphir.ir.Type.Reference(
              {},
              (morphir.sdk.List(
                morphir.sdk.List("""morphir"""),
                morphir.sdk.List(
                  """s""",
                  """d""",
                  """k"""
                )
              ), morphir.sdk.List(morphir.sdk.List(
                """local""",
                """time"""
              )), morphir.sdk.List(
                """local""",
                """time"""
              )),
              morphir.sdk.List(
              
              )
            ) : morphir.ir.Type.Type[scala.Unit])
        }
    }
  
  def dependencyGraphFromEntryPointFunctions(
    entryPoints: morphir.sdk.Set.Set[morphir.ir.FQName.FQName]
  )(
    distrosMap: morphir.sdk.Dict.Dict[morphir.ir.Package.PackageName, morphir.ir.Distribution.Distribution]
  ): morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], morphir.dependency.DAG.DAG[(morphir.ir.Source.NodeType, morphir.ir.FQName.FQName)]] = {
    def collectDependenciesFromRefs(
      refs: morphir.sdk.Set.Set[(morphir.ir.Source.NodeType, morphir.ir.FQName.FQName)]
    )(
      dagResult: morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], morphir.dependency.DAG.DAG[(morphir.ir.Source.NodeType, morphir.ir.FQName.FQName)]]
    ): morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], morphir.dependency.DAG.DAG[(morphir.ir.Source.NodeType, morphir.ir.FQName.FQName)]] =
      morphir.sdk.Set.foldl(collectReferenceDependencies)(dagResult)(refs)
    
    def collectReferenceDependencies: ((morphir.ir.Source.NodeType, morphir.ir.FQName.FQName)) => morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], morphir.dependency.DAG.DAG[(morphir.ir.Source.NodeType, morphir.ir.FQName.FQName)]] => morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], morphir.dependency.DAG.DAG[(morphir.ir.Source.NodeType, morphir.ir.FQName.FQName)]] =
      ({
        case (nodeType, fQName) => 
          ((dagResultSoFar: morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], morphir.dependency.DAG.DAG[(morphir.ir.Source.NodeType, morphir.ir.FQName.FQName)]]) =>
            {
              def withErr(
                err: morphir.ir.Source.Error
              ): morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], morphir.dependency.DAG.DAG[(morphir.ir.Source.NodeType, morphir.ir.FQName.FQName)]] =
                dagResultSoFar match {
                  case morphir.sdk.Result.Ok(_) => 
                    (morphir.sdk.Result.Err(morphir.sdk.List(err)) : morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], morphir.dependency.DAG.DAG[(morphir.ir.Source.NodeType, morphir.ir.FQName.FQName)]])
                  case morphir.sdk.Result.Err(errs: morphir.sdk.List.List[morphir.ir.Source.Error]) =>
                    (morphir.sdk.Result.Err(morphir.sdk.List.cons(err)(errs)) : morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], morphir.dependency.DAG.DAG[(morphir.ir.Source.NodeType, morphir.ir.FQName.FQName)]])
                }
              
              def addNodeEdges(
                dagResultSoFarAcc: morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], morphir.dependency.DAG.DAG[(morphir.ir.Source.NodeType, morphir.ir.FQName.FQName)]]
              )(
                set: morphir.sdk.Set.Set[(morphir.ir.Source.NodeType, morphir.ir.FQName.FQName)]
              ): morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], morphir.dependency.DAG.DAG[(morphir.ir.Source.NodeType, morphir.ir.FQName.FQName)]] =
                morphir.sdk.Result.andThen(morphir.sdk.Basics.composeRight(morphir.dependency.DAG.insertNode((nodeType, fQName))(set))(morphir.sdk.Result.mapError(morphir.sdk.Basics.composeRight((morphir.ir.Source.CyclicDependency.apply : morphir.dependency.DAG.CycleDetected[(morphir.ir.Source.NodeType, morphir.ir.FQName.FQName)] => morphir.ir.Source.Error))(morphir.sdk.List.singleton))))(dagResultSoFarAcc)

              def isMorphirSDKFQN(
                fqn: morphir.ir.FQName.FQName
              ): morphir.sdk.Basics.Bool =
                morphir.sdk.Basics.equal(morphir.ir.FQName.getPackagePath(fqn))(morphir.sdk.List(
                  morphir.sdk.List("""morphir"""),
                  morphir.sdk.List(
                    """s""",
                    """d""",
                    """k"""
                  )
                ))
              
              if (isMorphirSDKFQN(fQName)) {
                dagResultSoFar
              } else {
                morphir.sdk.Dict.get(morphir.ir.FQName.getPackagePath(fQName))(distrosMap) match {
                  case morphir.sdk.Maybe.Just(distro: morphir.ir.Distribution.Distribution) =>
                    if (morphir.sdk.Basics.equal(nodeType)("""Value""")) {
                      morphir.ir.Distribution.lookupValueDefinition(fQName)(distro) match {
                        case morphir.sdk.Maybe.Just(valueDef: morphir.ir.Value.Definition[scala.Unit, morphir.ir.Type.Type[scala.Unit]]) =>
                          {
                            val directDependencies: morphir.sdk.Set.Set[(morphir.sdk.String.String, morphir.ir.FQName.FQName)] = morphir.sdk.Set.union[(morphir.sdk.String.String, morphir.ir.FQName.FQName)](morphir.sdk.Set.map(morphir.sdk.Tuple.pair[String, morphir.ir.FQName.FQName]("""Type"""))(morphir.sdk.Set.filter(morphir.sdk.Basics.composeRight(isMorphirSDKFQN)(morphir.sdk.Basics.not))(morphir.sdk.Set.union(morphir.ir.Value.collectTypeReferences(valueDef.body))(morphir.sdk.List.foldl(morphir.sdk.Set.union[morphir.ir.FQName.FQName])(morphir.sdk.Set.empty[morphir.ir.FQName.FQName])(morphir.sdk.List.map(morphir.ir.Type.collectReferences[scala.Unit])(morphir.sdk.List.cons(valueDef.outputType)(morphir.sdk.List.map(({
                              case (_, _, tpe) => 
                                tpe
                            } : ((morphir.ir.Name.Name, morphir.ir.Type.Type[scala.Unit], morphir.ir.Type.Type[scala.Unit])) => morphir.ir.Type.Type[scala.Unit]))(valueDef.inputTypes))))))))(morphir.sdk.Set.map(morphir.sdk.Tuple.pair[String, morphir.ir.FQName.FQName]("""Value"""))(morphir.sdk.Set.filter(morphir.sdk.Basics.composeRight(isMorphirSDKFQN)(morphir.sdk.Basics.not))(morphir.ir.Value.collectReferences(valueDef.body))))
                            
                            collectDependenciesFromRefs(directDependencies)(addNodeEdges(dagResultSoFar)(directDependencies))
                          }
                        case morphir.sdk.Maybe.Nothing => 
                          withErr((morphir.ir.Source.BrokenFunctionReference(fQName) : morphir.ir.Source.Error))
                      }
                    } else {
                      morphir.ir.Distribution.lookupTypeSpecification(fQName)(distro) match {
                        case morphir.sdk.Maybe.Just(morphir.ir.Type.TypeAliasSpecification(_, tpe)) => 
                          ((directDependencies: morphir.sdk.Set.Set[(morphir.ir.Source.NodeType, morphir.ir.FQName.FQName)]) =>
                            collectDependenciesFromRefs(directDependencies)(addNodeEdges(dagResultSoFar)(directDependencies)))(morphir.sdk.Set.map(morphir.sdk.Tuple.pair[String, morphir.ir.FQName.FQName]("""Type"""))(morphir.sdk.Set.filter(morphir.sdk.Basics.composeRight(isMorphirSDKFQN)(morphir.sdk.Basics.not))(morphir.ir.Type.collectReferences(tpe))))
                        case morphir.sdk.Maybe.Just(morphir.ir.Type.CustomTypeSpecification(_, ctors)) => 
                          ((directDependencies: morphir.sdk.Set.Set[(morphir.ir.Source.NodeType, morphir.ir.FQName.FQName)]) =>
                            collectDependenciesFromRefs(directDependencies)(addNodeEdges(dagResultSoFar)(directDependencies)))(morphir.sdk.Set.map(morphir.sdk.Tuple.pair[String, morphir.ir.FQName.FQName]("""Type"""))(morphir.sdk.Set.filter(morphir.sdk.Basics.composeRight(isMorphirSDKFQN)(morphir.sdk.Basics.not))(morphir.sdk.List.foldl(morphir.sdk.Set.union[morphir.ir.FQName.FQName])(morphir.sdk.Set.empty)(morphir.sdk.List.map(morphir.sdk.Basics.composeRight(morphir.sdk.Tuple.second[morphir.ir.Name.Name, morphir.ir.Type.Type[scala.Unit]])(morphir.ir.Type.collectReferences[scala.Unit]))(morphir.sdk.List.concat(morphir.sdk.Dict.values(ctors)))))))
                        case morphir.sdk.Maybe.Just(_) => 
                          dagResultSoFar
                        case morphir.sdk.Maybe.Nothing => 
                          withErr((morphir.ir.Source.BrokenFunctionReference(fQName) : morphir.ir.Source.Error))
                      }
                    }
                  case morphir.sdk.Maybe.Nothing => 
                    withErr((morphir.ir.Source.MissingPackage(morphir.ir.FQName.getPackagePath(fQName)) : morphir.ir.Source.Error))
                }
              }
            })
      } : ((morphir.ir.Source.NodeType, morphir.ir.FQName.FQName)) => morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], morphir.dependency.DAG.DAG[(morphir.ir.Source.NodeType, morphir.ir.FQName.FQName)]] => morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], morphir.dependency.DAG.DAG[(morphir.ir.Source.NodeType, morphir.ir.FQName.FQName)]])
    
    collectDependenciesFromRefs(morphir.sdk.Set.map(morphir.sdk.Tuple.pair[String, morphir.ir.FQName.FQName]("""Value"""))(entryPoints))(morphir.sdk.Set.foldl(((entryPoint: morphir.ir.FQName.FQName) =>
      ((acc: morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], morphir.dependency.DAG.DAG[(morphir.sdk.String.String, morphir.ir.FQName.FQName)]]) =>
        morphir.sdk.Result.andThen(((dag: morphir.dependency.DAG.DAG[(morphir.sdk.String.String, morphir.ir.FQName.FQName)]) =>
          morphir.sdk.Result.mapError(morphir.sdk.Basics.composeRight((morphir.ir.Source.CyclicDependency.apply : morphir.dependency.DAG.CycleDetected[(morphir.ir.Source.NodeType, morphir.ir.FQName.FQName)] => morphir.ir.Source.Error))(morphir.sdk.List.singleton))(morphir.dependency.DAG.insertNode(("""Value""", entryPoint))(morphir.sdk.Set.empty)(dag))))(acc))))((morphir.sdk.Result.Ok(morphir.dependency.DAG.empty) : morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], morphir.dependency.DAG.DAG[(morphir.sdk.String.String, morphir.ir.FQName.FQName)]]))(entryPoints))
  }
  
  def distrosByName(
    distros: morphir.sdk.List.List[morphir.ir.Distribution.Distribution]
  ): morphir.sdk.Dict.Dict[morphir.ir.Package.PackageName, morphir.ir.Distribution.Distribution] =
    morphir.sdk.Dict.fromList(morphir.sdk.List.map(((distro: morphir.ir.Distribution.Distribution) =>
      distro match {
        case morphir.ir.Distribution.Library(packageName, _, _) => 
          (packageName, distro)
      }))(distros))
  
  def outputSource(
    functionName: morphir.ir.FQName.FQName
  )(
    arguments: morphir.sdk.Dict.Dict[morphir.ir.Source.ParameterName, morphir.ir.Source.DataSourceName]
  ): morphir.ir.Source.OutputSource =
    morphir.ir.Source.OutputSource(
      arguments = arguments,
      functionReference = functionName
    )
  
  def outputSourcesToValue(
    distroMap: morphir.sdk.Dict.Dict[morphir.ir.Package.PackageName, morphir.ir.Distribution.Distribution]
  )(
    validDataSourceNames: morphir.sdk.Dict.Dict[morphir.ir.Source.DataSourceName, morphir.ir.Type.Type[scala.Unit]]
  )(
    outputName: morphir.ir.Source.OutputName
  )(
    outputSources: morphir.sdk.List.List[morphir.ir.Source.OutputSource]
  ): morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], morphir.ir.Value.TypedValue] = {
    val dataSourceValues: morphir.sdk.Dict.Dict[morphir.ir.Source.DataSourceName, morphir.ir.Value.TypedValue] = morphir.sdk.Dict.map(((srcName: morphir.ir.Name.Name) =>
      ((tpe: morphir.ir.Type.Type[scala.Unit]) =>
        (morphir.ir.Value.Variable(
          tpe,
          srcName
        ) : morphir.ir.Value.Value[scala.Unit, morphir.ir.Type.Type[scala.Unit]]))))(validDataSourceNames)
    
    def tpesToFunctionType(
      types: morphir.sdk.List.List[morphir.ir.Type.Type[scala.Unit]]
    )(
      outType: morphir.ir.Type.Type[scala.Unit]
    ): morphir.ir.Type.Type[scala.Unit] = {
      def helper(
        tpes: morphir.sdk.List.List[morphir.ir.Type.Type[scala.Unit]]
      )(
        out: morphir.ir.Type.Type[scala.Unit]
      ): morphir.ir.Type.Type[scala.Unit] =
        tpes match {
          case Nil => 
            out
          case (lastType :: rest) => 
            helper(rest)((morphir.ir.Type.Function(
              {},
              lastType,
              out
            ) : morphir.ir.Type.Type[scala.Unit]))
        }
      
      helper(morphir.sdk.List.reverse(types))(outType)
    }
    
    def outputSliceToValue(
      sliceIndex: morphir.sdk.Basics.Int
    )(
      outputSrc: morphir.ir.Source.OutputSource
    ): morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], morphir.ir.Value.TypedValue] =
      morphir.sdk.Result.map(({
        case (_, _val) => 
          _val
      } : ((morphir.sdk.List.List[morphir.ir.Type.Type[scala.Unit]], morphir.ir.Value.Value[scala.Unit, morphir.ir.Type.Type[scala.Unit]])) => morphir.ir.Value.Value[scala.Unit, morphir.ir.Type.Type[scala.Unit]]))(morphir.sdk.Result.andThen(((valDef: morphir.ir.Value.Definition[scala.Unit, morphir.ir.Type.Type[scala.Unit]]) =>
        {
          val inputTypes: morphir.sdk.List.List[morphir.ir.Type.Type[scala.Unit]] = morphir.sdk.List.map(({
            case (_, _, tpe) => 
              tpe
          } : ((morphir.ir.Source.ParameterName, morphir.ir.Type.Type[scala.Unit], morphir.ir.Type.Type[scala.Unit])) => morphir.ir.Type.Type[scala.Unit]))(valDef.inputTypes)
          
          morphir.sdk.List.foldl(({
            case (paramName, _, _) => 
              ((resSoFar: morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], (morphir.sdk.List.List[morphir.ir.Type.Type[scala.Unit]], morphir.ir.Value.Value[scala.Unit, morphir.ir.Type.Type[scala.Unit]])]) =>
                morphir.sdk.Maybe.andThen(((n: morphir.ir.Name.Name) =>
                  morphir.sdk.Dict.get(n)(dataSourceValues)))(morphir.sdk.Dict.get(paramName)(outputSrc.arguments)) match {
                  case morphir.sdk.Maybe.Just(paramValue) => 
                    morphir.sdk.Result.map(({
                      case (inTpes, targetVal) => 
                        {
                          val tpesWithoutCurrentParamTpe: morphir.sdk.List.List[morphir.ir.Type.Type[scala.Unit]] = morphir.sdk.List.drop(morphir.sdk.Basics.Int(1))(inTpes)
                          
                          val applyTpe: morphir.ir.Type.Type[scala.Unit] = tpesToFunctionType(tpesWithoutCurrentParamTpe)(valDef.outputType)
                          
                          (tpesWithoutCurrentParamTpe, (morphir.ir.Value.Apply(
                            applyTpe,
                            targetVal,
                            paramValue
                          ) : morphir.ir.Value.Value[scala.Unit, morphir.ir.Type.Type[scala.Unit]]))
                        }
                    } : ((morphir.sdk.List.List[morphir.ir.Type.Type[scala.Unit]], morphir.ir.Value.Value[scala.Unit, morphir.ir.Type.Type[scala.Unit]])) => (morphir.sdk.List.List[morphir.ir.Type.Type[scala.Unit]], morphir.ir.Value.Value[scala.Unit, morphir.ir.Type.Type[scala.Unit]])))(resSoFar)
                  case morphir.sdk.Maybe.Nothing => 
                    resSoFar match {
                      case morphir.sdk.Result.Err(errs: morphir.sdk.List.List[morphir.ir.Source.Error]) =>
                        (morphir.sdk.Result.Err(morphir.sdk.List.cons((morphir.ir.Source.ParamNotSupplied(
                          outputName,
                          sliceIndex,
                          outputSrc.functionReference,
                          paramName
                        ) : morphir.ir.Source.Error))(errs)) : morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], (morphir.sdk.List.List[morphir.ir.Type.Type[scala.Unit]], morphir.ir.Value.Value[scala.Unit, morphir.ir.Type.Type[scala.Unit]])])
                      case _ => 
                        (morphir.sdk.Result.Err(morphir.sdk.List((morphir.ir.Source.ParamNotSupplied(
                          outputName,
                          sliceIndex,
                          outputSrc.functionReference,
                          paramName
                        ) : morphir.ir.Source.Error))) : morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], (morphir.sdk.List.List[morphir.ir.Type.Type[scala.Unit]], morphir.ir.Value.Value[scala.Unit, morphir.ir.Type.Type[scala.Unit]])])
                    }
                })
          } : ((morphir.ir.Name.Name, morphir.ir.Type.Type[scala.Unit], morphir.ir.Type.Type[scala.Unit])) => morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], (morphir.sdk.List.List[morphir.ir.Type.Type[scala.Unit]], morphir.ir.Value.Value[scala.Unit, morphir.ir.Type.Type[scala.Unit]])] => morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], (morphir.sdk.List.List[morphir.ir.Type.Type[scala.Unit]], morphir.ir.Value.Value[scala.Unit, morphir.ir.Type.Type[scala.Unit]])]))((morphir.sdk.Result.Ok((inputTypes, (morphir.ir.Value.Reference(
            tpesToFunctionType(inputTypes)(valDef.outputType),
            outputSrc.functionReference
          ) : morphir.ir.Value.Value[scala.Unit, morphir.ir.Type.Type[scala.Unit]]))) : morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], (morphir.sdk.List.List[morphir.ir.Type.Type[scala.Unit]], morphir.ir.Value.Value[scala.Unit, morphir.ir.Type.Type[scala.Unit]])]))(valDef.inputTypes)
        }))(morphir.sdk.Result.fromMaybe(morphir.sdk.List((morphir.ir.Source.BrokenFunctionReference(outputSrc.functionReference) : morphir.ir.Source.Error)))(morphir.sdk.Maybe.andThen(morphir.ir.Distribution.lookupValueDefinition(outputSrc.functionReference))(morphir.sdk.Dict.get(morphir.ir.FQName.getPackagePath(outputSrc.functionReference))(distroMap)))))
    
    morphir.sdk.Result.map(((v: morphir.sdk.List.List[morphir.ir.Value.Value[scala.Unit, morphir.ir.Type.Type[scala.Unit]]]) =>
      v match {
        case Nil => 
          (morphir.ir.Value.Unit((morphir.ir.Type.Unit({}) : morphir.ir.Type.Type[scala.Unit])) : morphir.ir.Value.Value[scala.Unit, morphir.ir.Type.Type[scala.Unit]])
        case (head :: _) => 
          {
            val outTpe: morphir.ir.Type.Type[Unit] = morphir.ir.Value.valueAttribute(head)
            
            (morphir.ir.Value.Apply(
              outTpe,
              (morphir.ir.Value.Reference(
                (morphir.ir.Type.Function(
                  {},
                  (morphir.ir.Type.Reference(
                    {},
                    morphir.ir.FQName.fqn("""Morphir.SDK""")("""List""")("""List"""),
                    morphir.sdk.List(outTpe)
                  ) : morphir.ir.Type.Type[scala.Unit]),
                  outTpe
                ) : morphir.ir.Type.Type[scala.Unit]),
                morphir.ir.FQName.fqn("""Morphir.SDK""")("""List""")("""concat""")
              ) : morphir.ir.Value.Value[scala.Unit, morphir.ir.Type.Type[scala.Unit]]),
              (morphir.ir.Value.List(
                (morphir.ir.Type.Reference(
                  {},
                  morphir.ir.FQName.fqn("""Morphir.SDK""")("""List""")("""List"""),
                  morphir.sdk.List(outTpe)
                ) : morphir.ir.Type.Type[scala.Unit]),
                v
              ) : morphir.ir.Value.Value[scala.Unit, morphir.ir.Type.Type[scala.Unit]])
            ) : morphir.ir.Value.Value[scala.Unit, morphir.ir.Type.Type[scala.Unit]])
          }
      }))(morphir.sdk.List.foldl(((res: morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], morphir.ir.Value.Value[scala.Unit, morphir.ir.Type.Type[scala.Unit]]]) =>
      ((acc: morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], morphir.sdk.List.List[morphir.ir.Value.Value[scala.Unit, morphir.ir.Type.Type[scala.Unit]]]]) =>
        (res, acc) match {
          case (morphir.sdk.Result.Ok(v), morphir.sdk.Result.Ok(accSoFar)) => 
            (morphir.sdk.Result.Ok(morphir.sdk.List.cons(v)(accSoFar)) : morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], morphir.sdk.List.List[morphir.ir.Value.Value[scala.Unit, morphir.ir.Type.Type[scala.Unit]]]])
          case (morphir.sdk.Result.Ok(_), morphir.sdk.Result.Err(err)) => 
            (morphir.sdk.Result.Err(err) : morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], morphir.sdk.List.List[morphir.ir.Value.Value[scala.Unit, morphir.ir.Type.Type[scala.Unit]]]])
          case (morphir.sdk.Result.Err(errs), morphir.sdk.Result.Ok(_)) => 
            (morphir.sdk.Result.Err(errs) : morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], morphir.sdk.List.List[morphir.ir.Value.Value[scala.Unit, morphir.ir.Type.Type[scala.Unit]]]])
          case (morphir.sdk.Result.Err(resErrs), morphir.sdk.Result.Err(accErrs)) => 
            (morphir.sdk.Result.Err(morphir.sdk.Basics.append(resErrs)(accErrs)) : morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], morphir.sdk.List.List[morphir.ir.Value.Value[scala.Unit, morphir.ir.Type.Type[scala.Unit]]]])
        })))((morphir.sdk.Result.Ok(morphir.sdk.List(
    
    )) : morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], morphir.sdk.List.List[morphir.ir.Value.Value[scala.Unit, morphir.ir.Type.Type[scala.Unit]]]]))(morphir.sdk.List.indexedMap(outputSliceToValue)(outputSources)))
  }
  
  def toDistributionComponent(
    distros: morphir.sdk.List.List[morphir.ir.Distribution.Distribution]
  )(
    comp: morphir.ir.Source.Component
  ): morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], morphir.ir.Distribution.Component] = {
    val entryPoints: morphir.sdk.Set.Set[morphir.ir.FQName.FQName] = morphir.sdk.Dict.foldl(({
      case _ => 
        ((outputSources: morphir.sdk.List.List[morphir.ir.Source.OutputSource]) =>
          ((acc: morphir.sdk.Set.Set[morphir.ir.FQName.FQName]) =>
            morphir.sdk.Set.union(acc)(morphir.sdk.Set.fromList(morphir.sdk.List.map(((x: morphir.ir.Source.OutputSource) =>
              x.functionReference))(outputSources)))))
    } : morphir.ir.Name.Name => morphir.sdk.List.List[morphir.ir.Source.OutputSource] => morphir.sdk.Set.Set[morphir.ir.FQName.FQName] => morphir.sdk.Set.Set[morphir.ir.FQName.FQName]))(morphir.sdk.Set.empty)(comp.outputs)
    
    val inputs: morphir.sdk.Dict.Dict[morphir.ir.Name.Name, morphir.ir.Type.Type[scala.Unit]] = morphir.sdk.Dict.map(({
      case _ => 
        morphir.ir.Source.dataTypeToType
    } : morphir.ir.Name.Name => morphir.ir.Source.DataType => morphir.ir.Type.Type[scala.Unit]))(comp.inputs)
    
    val states: morphir.sdk.Dict.Dict[morphir.ir.Name.Name, morphir.ir.Type.Type[scala.Unit]] = morphir.sdk.Dict.map(({
      case _ => 
        morphir.ir.Source.dataTypeToType
    } : morphir.ir.Name.Name => morphir.ir.Source.DataType => morphir.ir.Type.Type[scala.Unit]))(comp.states)
    
    val outputs: morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], morphir.sdk.Dict.Dict[morphir.ir.Name.Name, morphir.ir.Value.Value[scala.Unit, morphir.ir.Type.Type[scala.Unit]]]] = morphir.sdk.Dict.foldl(((outputName: morphir.ir.Name.Name) =>
      ((outputSources: morphir.sdk.List.List[morphir.ir.Source.OutputSource]) =>
        ((outputResultSoFar: morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], morphir.sdk.Dict.Dict[morphir.ir.Name.Name, morphir.ir.Value.TypedValue]]) =>
          morphir.sdk.Result.map2 (((outputSoFar: morphir.sdk.Dict.Dict[morphir.ir.Name.Name, morphir.ir.Value.TypedValue], v: morphir.ir.Value.TypedValue) =>
              morphir.sdk.Dict.insert(outputName)(v)(outputSoFar)))(outputResultSoFar)(morphir.ir.Source.outputSourcesToValue(morphir.ir.Source.distrosByName(distros))(morphir.sdk.Dict.union(inputs)(states))(outputName)(outputSources))))))((morphir.sdk.Result.Ok(morphir.sdk.Dict.empty) : morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], morphir.sdk.Dict.Dict[morphir.ir.Name.Name, morphir.ir.Value.Value[scala.Unit, morphir.ir.Type.Type[scala.Unit]]]]))(comp.outputs)
    
    def createDistributionComponent(
      treeShakenDistros: morphir.sdk.Dict.Dict[morphir.ir.Package.PackageName, morphir.ir.Distribution.Distribution]
    ): morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], morphir.ir.Distribution.Component] =
      morphir.sdk.Result.map(((outputValues: morphir.sdk.Dict.Dict[morphir.ir.Name.Name, morphir.ir.Value.Value[scala.Unit, morphir.ir.Type.Type[scala.Unit]]]) =>
        morphir.ir.Distribution.Component(
          inputs = inputs,
          libraries = morphir.sdk.Dict.map(({
            case _ => 
              ({
                case morphir.ir.Distribution.Library(_, _, pkgDef) => 
                  pkgDef
              } : morphir.ir.Distribution.Distribution => morphir.ir.Package.Definition[scala.Unit, morphir.ir.Type.Type[scala.Unit]])
          } : morphir.ir.Package.PackageName => morphir.ir.Distribution.Distribution => morphir.ir.Package.Definition[scala.Unit, morphir.ir.Type.Type[scala.Unit]]))(treeShakenDistros),
          name = comp.name,
          outputs = outputValues,
          states = states
        )))(outputs)
    
    morphir.sdk.List.concat(morphir.sdk.List(
      morphir.ir.Source.collectNonUniqueDistributionErrors(distros),
      morphir.ir.Source.collectNonInputReferenceErrors(comp),
      morphir.ir.Source.collectInputAndStateNameConflictErrors(comp),
      morphir.ir.Source.collectOutputSliceTypeMismatchErrors(comp)(distros),
      morphir.ir.Source.collectUnusedInputStateErrors(comp)
    )) match {
      case Nil => 
        morphir.sdk.Result.andThen(createDistributionComponent)(morphir.ir.Source.treeShakeDistributions(entryPoints)(distros))
      case errs => 
        (morphir.sdk.Result.Err(errs) : morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], morphir.ir.Distribution.Component])
    }
  }
  
  def treeShakeDistributions(
    entryPoints: morphir.sdk.Set.Set[morphir.ir.FQName.FQName]
  )(
    distros: morphir.sdk.List.List[morphir.ir.Distribution.Distribution]
  ): morphir.sdk.Result.Result[morphir.sdk.List.List[morphir.ir.Source.Error], morphir.sdk.Dict.Dict[morphir.ir.Package.PackageName, morphir.ir.Distribution.Distribution]] = {
    val distrosMap = morphir.ir.Source.distrosByName(distros)
    
    morphir.sdk.Result.map(((dag: morphir.dependency.DAG.DAG[(morphir.ir.Source.NodeType, morphir.ir.FQName.FQName)]) =>
      {
        val dagNodes: morphir.sdk.Set.Set[(morphir.ir.Source.NodeType, morphir.ir.FQName.FQName)] = morphir.sdk.List.foldl(({
          case (fromNode, toNodes) => 
            ((nodesSoFar: morphir.sdk.Set.Set[(morphir.ir.Source.NodeType, morphir.ir.FQName.FQName)]) =>
              morphir.sdk.Set.union(nodesSoFar)(morphir.sdk.Set.insert(fromNode)(toNodes)))
        } : (((morphir.ir.Source.NodeType, morphir.ir.FQName.FQName), morphir.sdk.Set.Set[(morphir.ir.Source.NodeType, morphir.ir.FQName.FQName)])) => morphir.sdk.Set.Set[(morphir.ir.Source.NodeType, morphir.ir.FQName.FQName)] => morphir.sdk.Set.Set[(morphir.ir.Source.NodeType, morphir.ir.FQName.FQName)]))(morphir.sdk.Set.empty)(morphir.dependency.DAG.toList(dag))
        
        morphir.sdk.Dict.filter(({
          case _ => 
            ({
              case morphir.ir.Distribution.Library(_, _, pkgDef) => 
                morphir.sdk.Basics.not(morphir.sdk.Dict.isEmpty(pkgDef.modules))
            } : morphir.ir.Distribution.Distribution => morphir.sdk.Basics.Bool)
        } : morphir.ir.Path.Path => morphir.ir.Distribution.Distribution => morphir.sdk.Basics.Bool))(morphir.sdk.Dict.map(((packageName: morphir.ir.Path.Path) =>
          ({
            case morphir.ir.Distribution.Library(_, _, pkgDef) => 
              {
                val treeShakenModules: morphir.sdk.Dict.Dict[morphir.ir.Module.ModuleName, morphir.ir.AccessControlled.AccessControlled[morphir.ir.Module.Definition[scala.Unit, morphir.ir.Type.Type[scala.Unit]]]] = morphir.sdk.Dict.filter(({
                  case _ => 
                    ((accessCntrldModuleDef: morphir.ir.AccessControlled.AccessControlled[morphir.ir.Module.Definition[scala.Unit, morphir.ir.Type.Type[scala.Unit]]]) =>
                      morphir.sdk.Basics.not(morphir.sdk.Basics.and(morphir.sdk.Dict.isEmpty(accessCntrldModuleDef.value.types))(morphir.sdk.Dict.isEmpty(accessCntrldModuleDef.value.values))))
                } : morphir.ir.Path.Path => morphir.ir.AccessControlled.AccessControlled[morphir.ir.Module.Definition[scala.Unit, morphir.ir.Type.Type[scala.Unit]]] => morphir.sdk.Basics.Bool))(morphir.sdk.Dict.map(((moduleName: morphir.ir.Path.Path) =>
                  ((accessCntrldModuleDef: morphir.ir.AccessControlled.AccessControlled[morphir.ir.Module.Definition[scala.Unit, morphir.ir.Type.Type[scala.Unit]]]) =>
                    {
                      val treeShakenTypes: morphir.sdk.Dict.Dict[morphir.ir.Name.Name, morphir.ir.AccessControlled.AccessControlled[morphir.ir.Documented.Documented[morphir.ir.Type.Definition[scala.Unit]]]] = morphir.sdk.Dict.filter(((name: morphir.ir.Name.Name) =>
                        ({
                          case _ => 
                            morphir.sdk.Set.member(("""Type""", morphir.ir.FQName.fQName(packageName)(moduleName)(name)))(dagNodes)
                        } : morphir.ir.AccessControlled.AccessControlled[morphir.ir.Documented.Documented[morphir.ir.Type.Definition[scala.Unit]]] => morphir.sdk.Basics.Bool)))(accessCntrldModuleDef.value.types)
                      
                      val treeShakenValues: morphir.sdk.Dict.Dict[morphir.ir.Name.Name, morphir.ir.AccessControlled.AccessControlled[morphir.ir.Documented.Documented[morphir.ir.Value.Definition[scala.Unit, morphir.ir.Type.Type[scala.Unit]]]]] = morphir.sdk.Dict.filter(((name: morphir.ir.Name.Name) =>
                        ({
                          case _ => 
                            morphir.sdk.Set.member(("""Value""", morphir.ir.FQName.fQName(packageName)(moduleName)(name)))(dagNodes)
                        } : morphir.ir.AccessControlled.AccessControlled[morphir.ir.Documented.Documented[morphir.ir.Value.Definition[scala.Unit, morphir.ir.Type.Type[scala.Unit]]]] => morphir.sdk.Basics.Bool)))(accessCntrldModuleDef.value.values)
                      
                      morphir.ir.AccessControlled.AccessControlled(
                        access = accessCntrldModuleDef.access,
                        value = morphir.ir.Module.Definition(
                          doc = accessCntrldModuleDef.value.doc,
                          types = treeShakenTypes,
                          values = treeShakenValues
                        )
                      )
                    })))(pkgDef.modules))
                
                (morphir.ir.Distribution.Library(
                  packageName,
                  morphir.sdk.Dict.empty,
                  morphir.ir.Package.Definition(modules = treeShakenModules)
                ) : morphir.ir.Distribution.Distribution)
              }
          } : morphir.ir.Distribution.Distribution => morphir.ir.Distribution.Distribution)))(distrosMap))
      }))(morphir.ir.Source.dependencyGraphFromEntryPointFunctions(entryPoints)(distrosMap))
  }

}