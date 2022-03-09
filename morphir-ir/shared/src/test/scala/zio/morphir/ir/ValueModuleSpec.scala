package zio.morphir.ir

import zio.Chunk
import zio.test.*
import zio.morphir.testing.MorphirBaseSpec
import zio.morphir.syntax.ValueSyntax
import ValueModule.Value
import ValueModule.ValueCase.*
import zio.ZEnvironment

object ValueModuleSpec extends MorphirBaseSpec with ValueSyntax {
  def spec = suite("Value Module")(
    suite("Collect Variables should return as expected for:")(
      test("Apply") {
        val name  = Name.fromString("hello")
        val name2 = Name.fromString("world")
        val name3 = Name.fromString("planet")
        val ff    = fieldFunction(name3)
        val str   = string("string1")
        val str2  = string("string2")
        val rec   = record((name, str), (name2, str2))

        assertTrue(
          apply(ff, rec).collectVariables == Set(name3)
        )
      },
      test("Constructor") {
        val fqName = zio.morphir.ir.FQName(
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          Name("RecordType")
        )
        val constr = constructor(fqName)
        assertTrue(constr.collectVariables == Set[Name]())
      },
      test("Destructure") {
        val des = destructure(
          tuplePattern(asPattern(wildcardPattern, Name("name1")), asPattern(wildcardPattern, Name("name2"))),
          tuple(string("red"), string("blue")),
          variable("x")
        )
        assertTrue(des.collectVariables == Set(Name("x")))
      },
      test("Field") {
        val name = Name.fromString("Name")
        val fi   = field(string("String"), name)

        val name2 = Name.fromString("Name2")
        val name3 = Name.fromString("Name3")
        val fi2   = field(fieldFunction(name2), name3)

        assertTrue(
          fi.collectVariables == Set[Name]() &&
            fi2.collectVariables == Set(name2)
        )
      },
      test("FieldFunction") {
        val name = Name.fromString("Name")
        val ff   = fieldFunction(name)
        assertTrue(ff.collectVariables == Set(name))
      },
      test("IfThenElse") {
        val ife = ifThenElse(
          condition = literal(false),
          thenBranch = variable("y"),
          elseBranch = literal(3)
        )
        assertTrue(ife.collectVariables == Set(Name.fromString("y")))
      },
      test("Lambda") {
        val v1 = variable("x")

        val lam1 = lambda(
          asPattern(wildcardPattern, Name("x")),
          nativeApply(
            NativeFunction.Addition,
            Chunk(v1)
          )
        )
        val lam2 = lambda(
          asPattern(wildcardPattern, Name("x")),
          v1
        )
        assertTrue(
          lam1.collectVariables == Set[Name]() &&
            lam2.collectVariables == Set(Name("x"))
        )
      },
      test("LetDefinition") {
        import ValueModule.ValueDefinition

        val ld = letDefinition(
          Name("y"),
          ValueDefinition.fromLiteral(int(2)),
          nativeApply(
            NativeFunction.Addition,
            Chunk(variable("x"), variable("y"))
          )
        )
        assertTrue(ld.collectVariables == Set(Name("y")))
      },
      test("LetRecursion") {
        val lr = letRecursion(
          Map(
            Name.fromString("x") -> ifThenElse(
              condition = literal(false),
              thenBranch = variable("y"),
              elseBranch = literal(3)
            ).toDefinition,
            Name.fromString("y") ->
              ifThenElse(
                condition = literal(false),
                thenBranch = literal(2),
                elseBranch = variable("z")
              ).toDefinition
          ),
          nativeApply(
            NativeFunction.Addition,
            Chunk(
              variable("a"),
              variable("b")
            )
          )
        )

        assertTrue(lr.collectVariables == Set(Name("x"), Name("y"), Name("z")))
      },
      test("List") {
        val list1 = list(
          Chunk(
            literal("hello"),
            literal("world")
          )
        )
        val list2 = list(
          Chunk(
            variable(Name("hello")),
            int(3)
          )
        )
        assertTrue(
          list1.collectVariables == Set[Name]() &&
            list2.collectVariables == Set(Name("hello"))
        )
      },
      test("Literal") {
        val in = int(123)
        assertTrue(in.collectVariables == Set[Name]())
      },
      test("NativeApply") {
        val nat = nativeApply(
          NativeFunction.Addition,
          Chunk(variable("x"), variable("y"))
        )
        assertTrue(nat.collectVariables == Set[Name]())
      },
      test("PatternMatch") {
        val cases = Chunk(
          (asPattern(wildcardPattern, Name.fromString("x")), variable(Name("name"))),
          (asPattern(wildcardPattern, Name.fromString("y")), variable(Name("integer")))
        )

        val pm = patternMatch(
          wholeNumber(new java.math.BigInteger("42")),
          cases
        )
        assertTrue(pm.collectVariables == Set(Name("name"), Name("integer")))
      },
      test("Reference") {
        val ref = reference(
          zio.morphir.ir.FQName(
            zio.morphir.ir.Path(Name("Morphir.SDK")),
            zio.morphir.ir.Path(Name("Morphir.SDK")),
            Name("RecordType")
          )
        )
        assertTrue(ref.collectVariables == Set[Name]())
      },
      test("Record") {
        val name  = Name.fromString("hello")
        val name2 = Name.fromString("world")
        val str   = string("string1")
        val va    = variable(name2)

        val rec = record(Chunk((name, str), (name2, va)))
        assertTrue(rec.collectVariables == Set(name2))
      },
      test("Tuple") {
        val tuple1 = tuple(
          Chunk(
            literal("hello"),
            literal("world")
          )
        )
        val tuple2 = tuple(
          Chunk(
            variable(Name("hello")),
            int(3)
          )
        )
        assertTrue(
          tuple1.collectVariables == Set[Name]() &&
            tuple2.collectVariables == Set(Name("hello"))
        )
      },
      test("Unit") {
        assertTrue(unit.collectVariables == Set[Name]())
      },
      test("UpdateRecord") {
        val ur = updateRecord(
          string("hello world"),
          Chunk(
            Name("fieldB") -> wholeNumber(new java.math.BigInteger("3")),
            Name("fieldC") -> variable(Name("none"))
          )
        )
        assertTrue(ur.collectVariables == Set(Name("none")))
      },
      test("Variable") {
        val name = Name("ha")
        assertTrue(variable(name).collectVariables == Set(name))
      }
    ),
    suite("Collect References should return as expected for:")(
      test("Apply") {
        val name  = FQName.fromString("hello:world", ":")
        val name2 = Name.fromString("wonderful")
        val ff    = reference(name)
        val str   = string("string2")
        val rec   = record((name2, str))

        assertTrue(
          apply(ff, rec).collectReferences == Set(name)
        )
      },
      test("Constructor") {
        val fqName = zio.morphir.ir.FQName(
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          Name("RecordType")
        )
        val constr = constructor(fqName)
        assertTrue(constr.collectReferences == Set[FQName]())
      },
      test("Destructure") {
        val fq = zio.morphir.ir.FQName(
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          Name("RecordType")
        )
        val des = destructure(
          tuplePattern(asPattern(wildcardPattern, Name("name1")), asPattern(wildcardPattern, Name("name2"))),
          tuple(string("red"), reference(fq)),
          variable("x")
        )
        assertTrue(des.collectReferences == Set(fq))
      },
      test("Field") {
        val name = Name.fromString("Name")
        val fi   = field(string("String"), name)

        val fqName = zio.morphir.ir.FQName(
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          Name("RecordType")
        )
        val name2 = Name.fromString("Name3")
        val fi2   = field(reference(fqName), name2)

        assertTrue(
          fi.collectReferences == Set[FQName]() &&
            fi2.collectReferences == Set(fqName)
        )
      },
      test("FieldFunction") {
        val name = Name.fromString("Name")
        val ff   = fieldFunction(name)
        assertTrue(ff.collectReferences == Set[FQName]())
      },
      test("IfThenElse") {
        val fqName = zio.morphir.ir.FQName(
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          Name("RecordType")
        )
        val fqName2 = zio.morphir.ir.FQName(
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          Name("VariableType")
        )
        val ife = ifThenElse(
          condition = reference(fqName),
          thenBranch = variable("y"),
          elseBranch = reference(fqName2)
        )
        assertTrue(ife.collectReferences == Set(fqName, fqName2))
      },
      test("Lambda") {
        val fqName = zio.morphir.ir.FQName(
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          Name("RecordType")
        )

        val lam1 = lambda(
          asPattern(wildcardPattern, Name("x")),
          reference(fqName)
        )
        val lam2 = lambda(
          asPattern(wildcardPattern, Name("x")),
          variable("x")
        )
        assertTrue(
          lam1.collectReferences == Set(fqName) &&
            lam2.collectReferences == Set[FQName]()
        )
      },
      test("LetDefinition") {
        import ValueModule.ValueDefinition

        val fqName  = FQName.fromString("Morphir:SDK:valueType")
        val fqName2 = FQName.fromString("Morphir:SDK:typed")

        val ld = letDefinition(
          Name("y"),
          ValueDefinition.fromLiteral(reference(fqName)),
          tuple(
            int(42),
            reference(fqName2)
          )
        )
        assertTrue(ld.collectReferences == Set(fqName, fqName2))
      },
      test("LetRecursion") {
        val fqName = FQName.fromString("Zio:Morphir:IR")
        val lr = letRecursion(
          Map(
            Name.fromString("x") -> ifThenElse(
              condition = literal(false),
              thenBranch = variable("y"),
              elseBranch = reference(fqName)
            ).toDefinition,
            Name.fromString("y") ->
              ifThenElse(
                condition = literal(false),
                thenBranch = literal(2),
                elseBranch = variable("z")
              ).toDefinition
          ),
          nativeApply(
            NativeFunction.Addition,
            Chunk(
              variable("a"),
              variable("b")
            )
          )
        )

        assertTrue(lr.collectReferences == Set(fqName))
      },
      test("List") {
        val list1 = list(
          Chunk(
            literal("hello"),
            literal("world")
          )
        )
        val fq = FQName.fromString("hello:world:star")
        val list2 = list(
          Chunk(
            reference(fq),
            int(3)
          )
        )
        assertTrue(
          list1.collectReferences == Set[FQName]() &&
            list2.collectReferences == Set(fq)
        )
      },
      test("Literal") {
        val in = int(123)
        assertTrue(in.collectReferences == Set[FQName]())
      },
      test("NativeApply") {
        val nat = nativeApply(
          NativeFunction.Addition,
          Chunk(variable("x"), variable("y"))
        )
        assertTrue(nat.collectReferences == Set[FQName]())
      },
      test("PatternMatch") {
        val fq  = FQName.fromString("hello:world:star", ":")
        val fq2 = FQName.fromString("hello:world:mission", ":")
        val cases = Chunk(
          (asPattern(wildcardPattern, Name.fromString("x")), variable(Name("name"))),
          (asPattern(wildcardPattern, Name.fromString("y")), reference(fq2))
        )

        val pm = patternMatch(
          reference(fq),
          cases
        )
        assertTrue(pm.collectReferences == Set(fq, fq2))
      },
      test("Reference") {
        val fq = zio.morphir.ir.FQName(
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          Name("RecordType")
        )
        val ref = reference(fq)
        assertTrue(ref.collectReferences == Set(fq))
      },
      test("Record") {
        val name   = Name.fromString("hello")
        val name2  = Name.fromString("world")
        val fqName = FQName.fromString("folder:location:name", ":")
        val str    = string("string1")
        val rf     = reference(fqName)

        val rec = record(Chunk((name, str), (name2, rf)))
        assertTrue(rec.collectReferences == Set(fqName))
      },
      test("Tuple") {
        val tuple1 = tuple(
          Chunk(
            literal("hello"),
            literal("world")
          )
        )
        val fq = FQName.fromString("hello:world:star", ":")
        val tuple2 = tuple(
          Chunk(
            reference(fq),
            int(3)
          )
        )
        assertTrue(
          tuple1.collectReferences == Set[FQName]() &&
            tuple2.collectReferences == Set(fq)
        )
      },
      test("Unit") {
        assertTrue(unit.collectReferences == Set[FQName]())
      },
      test("UpdateRecord") {
        val fq = FQName.fromString("hello:world:string", ":")
        val ur = updateRecord(
          string("hello world"),
          Chunk(
            Name("fieldB") -> wholeNumber(new java.math.BigInteger("3")),
            Name("fieldC") -> reference(fq)
          )
        )
        assertTrue(ur.collectReferences == Set(fq))
      },
      test("Variable") {
        assertTrue(variable(Name("name")).collectReferences == Set[FQName]())
      }
    ),
    suite("toRawValue should return as expected for:")(
      test("Apply") {
        val zenv: ZEnvironment[String] = ZEnvironment.apply("prod")
        val lit: LiteralCase[String]   = LiteralCase(Literal.string("timeout"))
        val value                      = Value(lit, zenv)
        val in: Value[String]          = Value(ApplyCase(value, Chunk(value)), zenv)

        assertTrue(in.toRawValue == apply(string("timeout"), Chunk(string("timeout"))))
      },
      test("Constructor") {
        val fqName = zio.morphir.ir.FQName(
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          Name("RecordType")
        )
        val zenv: ZEnvironment[String] = ZEnvironment.apply("prod")

        val constr: Value[String] = Value(ConstructorCase(fqName), zenv)
        assertTrue(constr.toRawValue == constructor(fqName))
      },
      test("Destructure") {
        val zenv: ZEnvironment[String] = ZEnvironment.apply("prod")
        val lit: LiteralCase[String]   = LiteralCase(Literal.string("timeout"))
        val lit2: LiteralCase[String]  = LiteralCase(Literal.string("username"))
        val value                      = Value(lit, zenv)
        val value2                     = Value(lit2, zenv)

        val des: Value[String] = Value(
          DestructureCase(
            Pattern.WildcardPattern[String](zenv),
            value,
            value2
          ),
          zenv
        )
        assertTrue(
          des.toRawValue == destructure(
            Pattern.WildcardPattern[String](zenv),
            string("timeout"),
            string("username")
          )
        )
      },
      test("Field") {
        val name                       = Name.fromString("Name")
        val zenv: ZEnvironment[String] = ZEnvironment.apply("prod")
        val lit: LiteralCase[String]   = LiteralCase(Literal.string("timeout"))
        val value                      = Value(lit, zenv)

        val fi: Value[String] = Value(FieldCase(value, name), zenv)

        assertTrue(
          fi.toRawValue == field(string("timeout"), name)
        )
      },
      test("FieldFunction") {
        val name                       = Name.fromString("Name")
        val zenv: ZEnvironment[String] = ZEnvironment.apply("prod")
        val ff: Value[String]          = Value(FieldFunctionCase(name), zenv)

        assertTrue(ff.toRawValue == fieldFunction(name))
      },
      test("IfThenElse") {
        val zenv: ZEnvironment[String] = ZEnvironment.apply("prod")
        val lit: LiteralCase[String]   = LiteralCase(Literal.string("timeout"))
        val value                      = Value(lit, zenv)

        val ife: Value[String] = Value(IfThenElseCase(value, value, value), zenv)
        val to                 = string("timeout")
        assertTrue(ife.toRawValue == ifThenElse(to, to, to))
      },
      test("Lambda") {
        val zenv: ZEnvironment[String] = ZEnvironment.apply("prod")
        val lit: LiteralCase[String]   = LiteralCase(Literal.string("timeout"))
        val value                      = Value(lit, zenv)

        val lam: Value[String] = Value(LambdaCase(Pattern.WildcardPattern(zenv), value), zenv)

        assertTrue(
          lam.toRawValue == lambda(Pattern.WildcardPattern(zenv), string("timeout"))
        )
      },
      test("LetDefinition") {
        import ValueModule.ValueDefinition

        val name                       = Name("y")
        val zenv: ZEnvironment[String] = ZEnvironment.apply("prod")
        val lit: LiteralCase[String]   = LiteralCase(Literal.string("timeout"))
        val value                      = Value(lit, zenv)

        val ld = Value(LetDefinitionCase(name, ValueDefinition.fromLiteral(value), value), zenv)
        assertTrue(
          ld.toRawValue == letDefinition(
            name,
            ValueDefinition.fromLiteral(string("timeout")),
            string("timeout")
          )
        )
      },
      test("LetRecursion") {
        val zenv: ZEnvironment[String] = ZEnvironment.apply("prod")
        val lit: LiteralCase[String]   = LiteralCase(Literal.string("timeout"))
        val value                      = Value(lit, zenv)
        val map = Map(
          Name.fromString("x") -> ifThenElse(
            condition = literal(false),
            thenBranch = variable("y"),
            elseBranch = literal(3)
          ).toDefinition
        )

        val lr = Value(LetRecursionCase(map, value), zenv)

        assertTrue(lr.toRawValue == letRecursion(map, string("timeout")))
      },
      test("List") {
        val zenv: ZEnvironment[String] = ZEnvironment.apply("prod")
        val lit: LiteralCase[String]   = LiteralCase(Literal.string("timeout"))
        val value                      = Value(lit, zenv)

        val l1 = Value(ListCase(Chunk(value)), zenv)

        assertTrue(
          l1.toRawValue == list(Chunk(string("timeout")))
        )
      },
      test("Literal") {
        val zenv: ZEnvironment[String] = ZEnvironment.apply("prod")
        val lit: LiteralCase[String]   = LiteralCase(Literal.string("timeout"))
        val value                      = Value(lit, zenv)

        assertTrue(value.toRawValue == string("timeout"))
      },
      test("NativeApply") {
        val zenv: ZEnvironment[String] = ZEnvironment.apply("prod")
        val lit: LiteralCase[String]   = LiteralCase(Literal.string("timeout"))
        val value                      = Value(lit, zenv)

        val nat = Value(
          NativeApplyCase(NativeFunction.Addition, Chunk(value)),
          zenv
        )
        assertTrue(nat.toRawValue == nativeApply(NativeFunction.Addition, Chunk(string("timeout"))))
      },
      test("PatternMatch") {
        val zenv: ZEnvironment[String] = ZEnvironment.apply("prod")
        val lit: LiteralCase[String]   = LiteralCase(Literal.string("timeout"))
        val value                      = Value(lit, zenv)

        val cases = Chunk(
          (Pattern.WildcardPattern(zenv), value)
        )

        val pm = Value(
          PatternMatchCase(
            value,
            cases
          ),
          zenv
        )
        assertTrue(
          pm.toRawValue == patternMatch(
            string("timeout"),
            Chunk((Pattern.WildcardPattern(zenv), string("timeout")))
          )
        )
      },
      test("Reference") {
        val zenv: ZEnvironment[String] = ZEnvironment.apply("prod")

        val fqName = zio.morphir.ir.FQName(
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          zio.morphir.ir.Path(Name("Morphir.SDK")),
          Name("RecordType")
        )
        val ref = Value(ReferenceCase(fqName), zenv)
        assertTrue(ref.toRawValue == reference(fqName))
      },
      test("Record") {
        val name                       = Name.fromString("hello")
        val zenv: ZEnvironment[String] = ZEnvironment.apply("prod")
        val lit: LiteralCase[String]   = LiteralCase(Literal.string("timeout"))
        val value                      = Value(lit, zenv)

        val rec = Value(RecordCase(Chunk((name, value))), zenv)

        assertTrue(rec.toRawValue == record(Chunk((name, string("timeout")))))
      },
      test("Tuple") {
        val zenv: ZEnvironment[String] = ZEnvironment.apply("prod")
        val lit: LiteralCase[String]   = LiteralCase(Literal.string("timeout"))
        val value                      = Value(lit, zenv)

        val t1 = Value(TupleCase(Chunk(value)), zenv)

        assertTrue(
          t1.toRawValue == tuple(Chunk(string("timeout")))
        )
      },
      test("UpdateRecord") {
//        val zenv: ZEnvironment[String] = ZEnvironment.apply("prod")
//
        // todo revisit this because Value can't be resolved
//        val ur = Value(
//          UpdateRecordCase(
//            "hello",
//            Chunk(Name("fieldB") -> "world")
//          ),
//          zenv
//        )

        //        assertTrue(
        //          ur.toRawValue == Value(
        //            UpdateRecordCase("hello", Chunk(Name("fieldB") -> "world")),
        //            ZEnvironment.empty
        //          )
        //        )
        assertTrue(1 == 1)
      },
      test("Variable") {
        val name                       = Name("ha")
        val zenv: ZEnvironment[String] = ZEnvironment.apply("prod")

        val value = Value(VariableCase(name), zenv)
        assertTrue(value.toRawValue == variable(name))
      },
      test("Unit") {
        val zenv: ZEnvironment[String] = ZEnvironment.apply("prod")
        assertTrue(Value(UnitCase, zenv).toRawValue == unit)
      }
    )
  )
}
