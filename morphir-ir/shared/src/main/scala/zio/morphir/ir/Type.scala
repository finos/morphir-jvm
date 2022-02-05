package zio.morphir.ir
import zio.{Chunk, ZEnvironment}

object Type {
  type Type[+Annotations] = MorphirIR.Type[Annotations]
  val Type = MorphirIR.Type

  type Field[+Annotations] = MorphirIR.Type.Field[Annotations]
  val Field = MorphirIR.Type.Field

  type Specification[+Annotations] = MorphirIR.TypeTree.Specification[Annotations]
  val Specification = MorphirIR.TypeTree.Specification

  type Definition[+Annotations] = MorphirIR.TypeTree.Definition[Annotations]
  val Definition = MorphirIR.TypeTree.Definition

  def definitionToSpecification[Annotations](definition: Definition[Annotations]): Specification[Annotations] =
    definition match {
      case Definition.TypeAliasDefinition(typeParams, typeExpr, annotations) =>
        Specification.TypeAliasSpecification(typeParams, typeExpr, annotations)
      case Definition.CustomTypeDefinition(params, accessControlledCtors, annotations) =>
        accessControlledCtors.fold(
          ifPublic = ctors => Specification.CustomTypeSpecification(params, ctors, annotations),
          ifPrivate = _ => Specification.OpaqueTypeSpecification(params, annotations)
        )
    }

  val unit: Type.Unit[Any] = Type.Unit(ZEnvironment.empty)
  def unit[Annotations](annotations: ZEnvironment[Annotations]): Type.Unit[Annotations] =
    Type.Unit(annotations)

  /**
   * Creates a type variable.
   * {{{
   *   toIR a ==  variable(Name.fromString("a"))
   *   toIR fooBar == variable(Name.fromString("fooBar"))
   * }}}
   */
  def variable[Annotations](name: Name, annotations: ZEnvironment[Annotations]): Type.Variable[Annotations] =
    Type.Variable(name, annotations)

  /**
   * Creates a type variable.
   * {{{
   *   toIR a ==  variable(Name.fromString("a"))
   *   toIR fooBar == variable(Name.fromString("fooBar"))
   * }}}
   */
  def variable(name: Name): Type.Variable[Any] =
    Type.Variable(name, ZEnvironment.empty)

  def variable(name: String): Type.Variable[Any] =
    Type.Variable(Name.fromString(name), ZEnvironment.empty)

  def reference[Annotations](
      name: FQName,
      typeParams: Chunk[Type[Annotations]],
      annotations: ZEnvironment[Annotations]
  ): Type.Reference[Annotations] =
    Type.Reference(name, typeParams, annotations)

  def reference(name: FQName, typeParams: Type[Any]*): Type.Reference[Any] =
    Type.Reference(name, Chunk.fromIterable(typeParams), ZEnvironment.empty)
}
