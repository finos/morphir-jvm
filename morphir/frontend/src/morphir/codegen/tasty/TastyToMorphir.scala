package morphir.codegen.tasty

import com.typesafe.scalalogging.{ Logger, StrictLogging }
import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.core.Contexts
import morphir.codegen.tasty.MorphUtils.*
import morphir.ir.{
  AccessControlled,
  Distribution,
  Documented,
  FQName,
  FormatVersion,
  Module,
  Name,
  Package,
  Path,
  Type as MorphType,
  Value
}
import morphir.sdk.Dict

import java.nio.file.{ Files, Paths }
import scala.quoted.*
import scala.tasty.inspector.*
import scala.util.{ Failure, Success, Try }

class TastyToMorphir(morphirPath: String) extends Inspector with StrictLogging {
  def inspect(using quotes: Quotes)(tastys: List[Tasty[quotes.type]]): Unit = {

    given Contexts.Context = quotes.asInstanceOf[runtime.impl.QuotesImpl].ctx

    val mergedDistribution = for {
      distributions <- tastys.collect {
                         case tasty if tasty.ast.isInstanceOf[Trees.Tree[?]] =>
                           tasty.ast.asInstanceOf[Trees.Tree[?]].toVersionedDistribution
                       }.toTryList
      commonRoot          <- findCommonPackageRoot(distributions)
      rebasedDistributions = distributions.map(rebaseDistribution(_, commonRoot))
      merged <- rebasedDistributions.reduceLeftOption((left, right) => mergeDistributions(left, right)) match {
                  case Some(distribution) => Success(distribution)
                  case None =>
                    Failure(UnsupportedOperationException(s"Could not merge distributions from inputs: $tastys"))
                }
    } yield merged

    mergedDistribution match {
      case Success(distribution) =>
        writeDistribution(distribution)
      case Failure(ex) =>
        logger.error(ex.getMessage, ex)
    }
  }

  private def mergeDistributions(
    left: FormatVersion.VersionedDistribution,
    right: FormatVersion.VersionedDistribution
  ): FormatVersion.VersionedDistribution = {
    val mergedDistribution =
      (left.distribution, right.distribution) match {
        case (
              Distribution.Distribution.Library(leftPackageName, leftDependencies, leftModules),
              Distribution.Distribution.Library(rightPackageName, rightDependencies, rightModules)
            ) =>
          if (leftPackageName != rightPackageName) {
            throw UnsupportedOperationException(
              s"Cannot merge distributions after rebasing because package ids still differ: $leftPackageName vs $rightPackageName"
            )
          }

          Distribution.Distribution.Library(
            leftPackageName,
            leftDependencies ++ rightDependencies,
            Package.Definition(
              modules = rightModules.modules.foldLeft(leftModules.modules) { case (acc, (moduleName, moduleDef)) =>
                acc.get(moduleName) match {
                  case Some(existing) =>
                    acc.updated(moduleName, mergeModuleDefinitions(existing, moduleDef))
                  case None =>
                    acc.updated(moduleName, moduleDef)
                }
              }
            )
          )
      }

    FormatVersion.VersionedDistribution(left.formatVersion, mergedDistribution)
  }

  private def findCommonPackageRoot(
    distributions: List[FormatVersion.VersionedDistribution]
  ): Try[Path.Path] =
    distributions match {
      case Nil =>
        Failure(UnsupportedOperationException("No distributions were produced from the provided TASTy inputs"))

      case head :: tail =>
        val allPackagePaths = distributions.map(packagePathOf)
        val commonRoot      = tail.map(packagePathOf).foldLeft(packagePathOf(head))(longestCommonPrefix)

        if (allPackagePaths.distinct.size > 1 && commonRoot.isEmpty) {
          Failure(
            UnsupportedOperationException(
              s"Cannot merge unrelated package roots: ${allPackagePaths.map(renderPath).mkString(", ")}"
            )
          )
        } else {
          Success(commonRoot)
        }
    }

  private def packagePathOf(distribution: FormatVersion.VersionedDistribution): Path.Path =
    distribution.distribution match {
      case Distribution.Distribution.Library(packagePath, _, _) =>
        packagePath
    }

  private def longestCommonPrefix(left: Path.Path, right: Path.Path): Path.Path =
    left.zip(right).takeWhile { case (leftName, rightName) => leftName == rightName }.map(_._1)

  private def renderPath(path: Path.Path): String =
    path.map(name => name.mkString("_")).mkString(".")

  private def rebaseDistribution(
    distribution: FormatVersion.VersionedDistribution,
    commonRoot: Path.Path
  ): FormatVersion.VersionedDistribution =
    distribution.distribution match {
      case Distribution.Distribution.Library(originalPackage, dependencies, modules) =>
        val packageSuffix = originalPackage.drop(commonRoot.length)
        val rebasedModules =
          modules.modules.foldLeft(
            Dict.empty[Module.ModuleName, AccessControlled.AccessControlled[
              Module.Definition[Unit, MorphType.Type[Unit]]
            ]]
          ) { case (acc, (moduleName, moduleDef)) =>
            acc.updated(packageSuffix ++ moduleName, rebaseModuleDefinition(moduleDef, commonRoot))
          }

        FormatVersion.VersionedDistribution(
          distribution.formatVersion,
          Distribution.Distribution.Library(
            commonRoot,
            dependencies,
            Package.Definition(rebasedModules)
          )
        )
    }

  private def rebaseModuleDefinition(
    moduleDef: AccessControlled.AccessControlled[Module.Definition[Unit, MorphType.Type[Unit]]],
    commonRoot: Path.Path
  ): AccessControlled.AccessControlled[Module.Definition[Unit, MorphType.Type[Unit]]] =
    moduleDef.copy(
      value = moduleDef.value.copy(
        types = moduleDef.value.types.foldLeft(
          Dict.empty[Name.Name, AccessControlled.AccessControlled[Documented.Documented[MorphType.Definition[Unit]]]]
        ) { case (acc, (typeName, typeDef)) =>
          acc.updated(
            typeName,
            typeDef.copy(value = typeDef.value.copy(value = rebaseTypeDefinition(typeDef.value.value, commonRoot)))
          )
        },
        values = moduleDef.value.values.foldLeft(
          Dict.empty[Name.Name, AccessControlled.AccessControlled[
            Documented.Documented[Value.Definition[Unit, MorphType.Type[Unit]]]
          ]]
        ) { case (acc, (valueName, valueDef)) =>
          acc.updated(
            valueName,
            valueDef.copy(value = valueDef.value.copy(value = rebaseValueDefinition(valueDef.value.value, commonRoot)))
          )
        }
      )
    )

  private def rebaseTypeDefinition(
    typeDef: MorphType.Definition[Unit],
    commonRoot: Path.Path
  ): MorphType.Definition[Unit] =
    typeDef match {
      case MorphType.TypeAliasDefinition(typeParams, tpe) =>
        MorphType.TypeAliasDefinition(typeParams, rebaseType(tpe, commonRoot))

      case MorphType.CustomTypeDefinition(typeParams, accessControlledConstructors) =>
        MorphType.CustomTypeDefinition(
          typeParams,
          accessControlledConstructors.copy(
            value =
              accessControlledConstructors.value.foldLeft(Dict.empty[Name.Name, MorphType.ConstructorArgs[Unit]]) {
                case (acc, (ctorName, ctorArgs)) =>
                  acc.updated(
                    ctorName,
                    ctorArgs.map { case (argName, argType) => (argName, rebaseType(argType, commonRoot)) }
                  )
              }
          )
        )
    }

  private def rebaseType(
    tpe: MorphType.Type[Unit],
    commonRoot: Path.Path
  ): MorphType.Type[Unit] =
    tpe match {
      case MorphType.Reference(attributes, fQName, typeArgs) =>
        MorphType.Reference(attributes, rebaseFQName(fQName, commonRoot), typeArgs.map(rebaseType(_, commonRoot)))

      case MorphType.Function(attributes, argType, returnType) =>
        MorphType.Function(attributes, rebaseType(argType, commonRoot), rebaseType(returnType, commonRoot))

      case MorphType.Record(attributes, fields) =>
        MorphType.Record(attributes, fields.map(field => field.copy(tpe = rebaseType(field.tpe, commonRoot))))

      case MorphType.ExtensibleRecord(attributes, subjectName, fields) =>
        MorphType.ExtensibleRecord(
          attributes,
          subjectName,
          fields.map(field => field.copy(tpe = rebaseType(field.tpe, commonRoot)))
        )

      case MorphType.Tuple(attributes, elements) =>
        MorphType.Tuple(attributes, elements.map(rebaseType(_, commonRoot)))

      case MorphType.Variable(_, _) | MorphType.Unit(_) =>
        tpe
    }

  private def rebaseValueDefinition(
    valueDef: Value.Definition[Unit, MorphType.Type[Unit]],
    commonRoot: Path.Path
  ): Value.Definition[Unit, MorphType.Type[Unit]] =
    valueDef.copy(
      inputTypes = valueDef.inputTypes.map { case (name, attributes, tpe) =>
        (name, rebaseType(attributes, commonRoot), rebaseType(tpe, commonRoot))
      },
      outputType = rebaseType(valueDef.outputType, commonRoot),
      body = rebaseValue(valueDef.body, commonRoot)
    )

  private def rebaseValue(
    value: Value.Value[Unit, MorphType.Type[Unit]],
    commonRoot: Path.Path
  ): Value.Value[Unit, MorphType.Type[Unit]] =
    value match {
      case Value.Value.Apply(attributes, function, argument) =>
        Value.Value.Apply(
          rebaseType(attributes, commonRoot),
          rebaseValue(function, commonRoot),
          rebaseValue(argument, commonRoot)
        )

      case Value.Value.Constructor(attributes, fQName) =>
        Value.Value.Constructor(rebaseType(attributes, commonRoot), rebaseFQName(fQName, commonRoot))

      case Value.Value.Destructure(attributes, pattern, valueToDestructure, inValue) =>
        Value.Value.Destructure(
          rebaseType(attributes, commonRoot),
          rebasePattern(pattern, commonRoot),
          rebaseValue(valueToDestructure, commonRoot),
          rebaseValue(inValue, commonRoot)
        )

      case Value.Value.Field(attributes, subjectValue, fieldName) =>
        Value.Value.Field(rebaseType(attributes, commonRoot), rebaseValue(subjectValue, commonRoot), fieldName)

      case Value.Value.FieldFunction(attributes, fieldName) =>
        Value.Value.FieldFunction(rebaseType(attributes, commonRoot), fieldName)

      case Value.Value.IfThenElse(attributes, condition, thenBranch, elseBranch) =>
        Value.Value.IfThenElse(
          rebaseType(attributes, commonRoot),
          rebaseValue(condition, commonRoot),
          rebaseValue(thenBranch, commonRoot),
          rebaseValue(elseBranch, commonRoot)
        )

      case Value.Value.Lambda(attributes, argumentPattern, body) =>
        Value.Value.Lambda(
          rebaseType(attributes, commonRoot),
          rebasePattern(argumentPattern, commonRoot),
          rebaseValue(body, commonRoot)
        )

      case Value.Value.LetDefinition(attributes, valueName, definition, inValue) =>
        Value.Value.LetDefinition(
          rebaseType(attributes, commonRoot),
          valueName,
          rebaseValueDefinition(definition, commonRoot),
          rebaseValue(inValue, commonRoot)
        )

      case Value.Value.LetRecursion(attributes, definitions, inValue) =>
        Value.Value.LetRecursion(
          rebaseType(attributes, commonRoot),
          definitions.foldLeft(Dict.empty[Name.Name, Value.Definition[Unit, MorphType.Type[Unit]]]) {
            case (acc, (valueName, definition)) =>
              acc.updated(valueName, rebaseValueDefinition(definition, commonRoot))
          },
          rebaseValue(inValue, commonRoot)
        )

      case Value.Value.List(attributes, items) =>
        Value.Value.List(rebaseType(attributes, commonRoot), items.map(rebaseValue(_, commonRoot)))

      case Value.Value.Literal(attributes, literal) =>
        Value.Value.Literal(rebaseType(attributes, commonRoot), literal)

      case Value.Value.PatternMatch(attributes, branchOn, cases) =>
        Value.Value.PatternMatch(
          rebaseType(attributes, commonRoot),
          rebaseValue(branchOn, commonRoot),
          cases.map { case (pattern, body) => (rebasePattern(pattern, commonRoot), rebaseValue(body, commonRoot)) }
        )

      case Value.Value.Record(attributes, fields) =>
        Value.Value.Record(
          rebaseType(attributes, commonRoot),
          fields.foldLeft(Dict.empty[Name.Name, Value.Value[Unit, MorphType.Type[Unit]]]) {
            case (acc, (fieldName, fieldValue)) =>
              acc.updated(fieldName, rebaseValue(fieldValue, commonRoot))
          }
        )

      case Value.Value.Reference(attributes, fQName) =>
        Value.Value.Reference(rebaseType(attributes, commonRoot), rebaseFQName(fQName, commonRoot))

      case Value.Value.Tuple(attributes, elements) =>
        Value.Value.Tuple(rebaseType(attributes, commonRoot), elements.map(rebaseValue(_, commonRoot)))

      case Value.Value.Unit(attributes) =>
        Value.Value.Unit(rebaseType(attributes, commonRoot))

      case Value.Value.UpdateRecord(attributes, valueToUpdate, fieldsToUpdate) =>
        Value.Value.UpdateRecord(
          rebaseType(attributes, commonRoot),
          rebaseValue(valueToUpdate, commonRoot),
          fieldsToUpdate.foldLeft(Dict.empty[Name.Name, Value.Value[Unit, MorphType.Type[Unit]]]) {
            case (acc, (fieldName, fieldValue)) =>
              acc.updated(fieldName, rebaseValue(fieldValue, commonRoot))
          }
        )

      case Value.Value.Variable(attributes, name) =>
        Value.Value.Variable(rebaseType(attributes, commonRoot), name)
    }

  private def rebasePattern(
    pattern: Value.Pattern[MorphType.Type[Unit]],
    commonRoot: Path.Path
  ): Value.Pattern[MorphType.Type[Unit]] =
    pattern match {
      case Value.Pattern.AsPattern(attributes, innerPattern, name) =>
        Value.Pattern.AsPattern(rebaseType(attributes, commonRoot), rebasePattern(innerPattern, commonRoot), name)

      case Value.Pattern.ConstructorPattern(attributes, fQName, args) =>
        Value.Pattern.ConstructorPattern(
          rebaseType(attributes, commonRoot),
          rebaseFQName(fQName, commonRoot),
          args.map(rebasePattern(_, commonRoot))
        )

      case Value.Pattern.EmptyListPattern(attributes) =>
        Value.Pattern.EmptyListPattern(rebaseType(attributes, commonRoot))

      case Value.Pattern.HeadTailPattern(attributes, headPattern, tailPattern) =>
        Value.Pattern.HeadTailPattern(
          rebaseType(attributes, commonRoot),
          rebasePattern(headPattern, commonRoot),
          rebasePattern(tailPattern, commonRoot)
        )

      case Value.Pattern.LiteralPattern(attributes, literal) =>
        Value.Pattern.LiteralPattern(rebaseType(attributes, commonRoot), literal)

      case Value.Pattern.TuplePattern(attributes, elements) =>
        Value.Pattern.TuplePattern(rebaseType(attributes, commonRoot), elements.map(rebasePattern(_, commonRoot)))

      case Value.Pattern.UnitPattern(attributes) =>
        Value.Pattern.UnitPattern(rebaseType(attributes, commonRoot))

      case Value.Pattern.WildcardPattern(attributes) =>
        Value.Pattern.WildcardPattern(rebaseType(attributes, commonRoot))
    }

  private def rebaseFQName(
    fQName: FQName.FQName,
    commonRoot: Path.Path
  ): FQName.FQName = {
    val (packagePath, modulePath, localName) = fQName

    if (packagePath.startsWith(commonRoot) && commonRoot.nonEmpty) {
      (commonRoot, packagePath.drop(commonRoot.length) ++ modulePath, localName)
    } else {
      fQName
    }
  }

  private def mergeModuleDefinitions(
    left: AccessControlled.AccessControlled[Module.Definition[Unit, MorphType.Type[Unit]]],
    right: AccessControlled.AccessControlled[Module.Definition[Unit, MorphType.Type[Unit]]]
  ): AccessControlled.AccessControlled[Module.Definition[Unit, MorphType.Type[Unit]]] = {
    val mergedAccess =
      if (left.access == AccessControlled.Access.Public || right.access == AccessControlled.Access.Public)
        AccessControlled.Access.Public
      else AccessControlled.Access.Private

    AccessControlled.AccessControlled(
      access = mergedAccess,
      value = Module.Definition(
        types = left.value.types ++ right.value.types,
        values = left.value.values ++ right.value.values,
        doc = left.value.doc.orElse(right.value.doc)
      )
    )
  }

  private def writeDistribution(distribution: FormatVersion.VersionedDistribution): Unit = {
    val encodedPackageDef = morphir.ir.formatversion.Codec.encodeVersionedDistribution(distribution)
    val jsonBytes         = encodedPackageDef.noSpaces.getBytes("UTF-8")
    Files.write(Paths.get(morphirPath), jsonBytes)
    logger.info(s"IR written to $morphirPath")
  }
}

@main def tastyToMorphirIR(morphirIROutputPath: String, tastyFiles: String*): Unit = {
  val logger = Logger("morphir.codegen.tasty.TastyToMorphir")
  logger.info(s"Provided IR Output Path: $morphirIROutputPath")
  logger.info(s"Provided TASTy files: $tastyFiles")

  val tastyFilesList = List(tastyFiles*)
  val tastyInspector = new TastyToMorphir(morphirIROutputPath)
  TastyInspector.inspectTastyFiles(tastyFilesList)(tastyInspector)
}
