package zio.morphir.ir.sdk

import zio.morphir.ir.Module
import zio.morphir.ir.Module.ModuleName
import zio.morphir.ir.Type.Specification.OpaqueTypeSpecification
import zio.morphir.ir.Type.Type._
import zio.morphir.ir.Type.{Type, UType}
import zio.morphir.ir.sdk.Common._
import zio.morphir.syntax.NamingSyntax._

object Key {
  val moduleName: ModuleName = ModuleName.fromString("Key")

  val moduleSpec: Module.USpecification = Module.USpecification(
    types = Map(
      name("Key0")  -> OpaqueTypeSpecification() ?? "",
      name("Key2")  -> OpaqueTypeSpecification() ?? "",
      name("Key3")  -> OpaqueTypeSpecification() ?? "",
      name("Key4")  -> OpaqueTypeSpecification() ?? "",
      name("Key5")  -> OpaqueTypeSpecification() ?? "",
      name("Key6")  -> OpaqueTypeSpecification() ?? "",
      name("Key7")  -> OpaqueTypeSpecification() ?? "",
      name("Key8")  -> OpaqueTypeSpecification() ?? "",
      name("Key9")  -> OpaqueTypeSpecification() ?? "",
      name("Key10") -> OpaqueTypeSpecification() ?? "",
      name("Key11") -> OpaqueTypeSpecification() ?? "",
      name("Key12") -> OpaqueTypeSpecification() ?? "",
      name("Key13") -> OpaqueTypeSpecification() ?? "",
      name("Key14") -> OpaqueTypeSpecification() ?? "",
      name("Key15") -> OpaqueTypeSpecification() ?? "",
      name("Key16") -> OpaqueTypeSpecification() ?? ""
    ),
    values = Map(
      vSpec("noKey")(key0Type),
      vSpec("key0")(key0Type),
      vSpec(
        "key2",
        "getKey1" -> tFun(tVar("a"))(tVar("b1")),
        "getKey2" -> tFun(tVar("a"))(tVar("b2")),
        "a"       -> tVar("a")
      )(key2Type(tVar("b1"), tVar("b2"))),
      vSpec(
        "key3",
        "getKey1" -> tFun(tVar("a"))(tVar("b1")),
        "getKey2" -> tFun(tVar("a"))(tVar("b2")),
        "getKey3" -> tFun(tVar("a"))(tVar("b3")),
        "a"       -> tVar("a")
      )(key3Type(tVar("b1"), tVar("b2"), tVar("b3"))),
      vSpec(
        "key4",
        "getKey1" -> tFun(tVar("a"))(tVar("b1")),
        "getKey2" -> tFun(tVar("a"))(tVar("b2")),
        "getKey3" -> tFun(tVar("a"))(tVar("b3")),
        "getKey4" -> tFun(tVar("a"))(tVar("b4")),
        "a"       -> tVar("a")
      )(key4Type(tVar("b1"), tVar("b2"), tVar("b3"), tVar("b4"))),
      vSpec(
        "key5",
        "getKey1" -> tFun(tVar("a"))(tVar("b1")),
        "getKey2" -> tFun(tVar("a"))(tVar("b2")),
        "getKey3" -> tFun(tVar("a"))(tVar("b3")),
        "getKey4" -> tFun(tVar("a"))(tVar("b4")),
        "getKey5" -> tFun(tVar("a"))(tVar("b5")),
        "a"       -> tVar("a")
      )(key5Type(tVar("b1"), tVar("b2"), tVar("b3"), tVar("b4"), tVar("b5"))),
      vSpec(
        "key6",
        "getKey1" -> tFun(tVar("a"))(tVar("b1")),
        "getKey2" -> tFun(tVar("a"))(tVar("b2")),
        "getKey3" -> tFun(tVar("a"))(tVar("b3")),
        "getKey4" -> tFun(tVar("a"))(tVar("b4")),
        "getKey5" -> tFun(tVar("a"))(tVar("b5")),
        "getKey6" -> tFun(tVar("a"))(tVar("b6")),
        "a"       -> tVar("a")
      )(key6Type(tVar("b1"), tVar("b2"), tVar("b3"), tVar("b4"), tVar("b5"), tVar("b6"))),
      vSpec(
        "key7",
        "getKey1" -> tFun(tVar("a"))(tVar("b1")),
        "getKey2" -> tFun(tVar("a"))(tVar("b2")),
        "getKey3" -> tFun(tVar("a"))(tVar("b3")),
        "getKey4" -> tFun(tVar("a"))(tVar("b4")),
        "getKey5" -> tFun(tVar("a"))(tVar("b5")),
        "getKey6" -> tFun(tVar("a"))(tVar("b6")),
        "getKey7" -> tFun(tVar("a"))(tVar("b7")),
        "a"       -> tVar("a")
      )(key7Type(tVar("b1"), tVar("b2"), tVar("b3"), tVar("b4"), tVar("b5"), tVar("b6"), tVar("b7"))),
      vSpec(
        "key8",
        "getKey1" -> tFun(tVar("a"))(tVar("b1")),
        "getKey2" -> tFun(tVar("a"))(tVar("b2")),
        "getKey3" -> tFun(tVar("a"))(tVar("b3")),
        "getKey4" -> tFun(tVar("a"))(tVar("b4")),
        "getKey5" -> tFun(tVar("a"))(tVar("b5")),
        "getKey6" -> tFun(tVar("a"))(tVar("b6")),
        "getKey7" -> tFun(tVar("a"))(tVar("b7")),
        "getKey8" -> tFun(tVar("a"))(tVar("b8")),
        "a"       -> tVar("a")
      )(key8Type(tVar("b1"), tVar("b2"), tVar("b3"), tVar("b4"), tVar("b5"), tVar("b6"), tVar("b7"), tVar("b8"))),
      vSpec(
        "key9",
        "getKey1" -> tFun(tVar("a"))(tVar("b1")),
        "getKey2" -> tFun(tVar("a"))(tVar("b2")),
        "getKey3" -> tFun(tVar("a"))(tVar("b3")),
        "getKey4" -> tFun(tVar("a"))(tVar("b4")),
        "getKey5" -> tFun(tVar("a"))(tVar("b5")),
        "getKey6" -> tFun(tVar("a"))(tVar("b6")),
        "getKey7" -> tFun(tVar("a"))(tVar("b7")),
        "getKey8" -> tFun(tVar("a"))(tVar("b8")),
        "getKey9" -> tFun(tVar("a"))(tVar("b9")),
        "a"       -> tVar("a")
      )(
        key9Type(
          tVar("b1"),
          tVar("b2"),
          tVar("b3"),
          tVar("b4"),
          tVar("b5"),
          tVar("b6"),
          tVar("b7"),
          tVar("b8"),
          tVar("b9")
        )
      ),
      vSpec(
        "key10",
        "getKey1"  -> tFun(tVar("a"))(tVar("b1")),
        "getKey2"  -> tFun(tVar("a"))(tVar("b2")),
        "getKey3"  -> tFun(tVar("a"))(tVar("b3")),
        "getKey4"  -> tFun(tVar("a"))(tVar("b4")),
        "getKey5"  -> tFun(tVar("a"))(tVar("b5")),
        "getKey6"  -> tFun(tVar("a"))(tVar("b6")),
        "getKey7"  -> tFun(tVar("a"))(tVar("b7")),
        "getKey8"  -> tFun(tVar("a"))(tVar("b8")),
        "getKey9"  -> tFun(tVar("a"))(tVar("b9")),
        "getKey10" -> tFun(tVar("a"))(tVar("b10")),
        "a"        -> tVar("a")
      )(
        key10Type(
          tVar("b1"),
          tVar("b2"),
          tVar("b3"),
          tVar("b4"),
          tVar("b5"),
          tVar("b6"),
          tVar("b7"),
          tVar("b8"),
          tVar("b9"),
          tVar("b10")
        )
      ),
      vSpec(
        "key11",
        "getKey1"  -> tFun(tVar("a"))(tVar("b1")),
        "getKey2"  -> tFun(tVar("a"))(tVar("b2")),
        "getKey3"  -> tFun(tVar("a"))(tVar("b3")),
        "getKey4"  -> tFun(tVar("a"))(tVar("b4")),
        "getKey5"  -> tFun(tVar("a"))(tVar("b5")),
        "getKey6"  -> tFun(tVar("a"))(tVar("b6")),
        "getKey7"  -> tFun(tVar("a"))(tVar("b7")),
        "getKey8"  -> tFun(tVar("a"))(tVar("b8")),
        "getKey9"  -> tFun(tVar("a"))(tVar("b9")),
        "getKey10" -> tFun(tVar("a"))(tVar("b10")),
        "getKey11" -> tFun(tVar("a"))(tVar("b11")),
        "a"        -> tVar("a")
      )(
        key11Type(
          tVar("b1"),
          tVar("b2"),
          tVar("b3"),
          tVar("b4"),
          tVar("b5"),
          tVar("b6"),
          tVar("b7"),
          tVar("b8"),
          tVar("b9"),
          tVar("b10"),
          tVar("b11")
        )
      ),
      vSpec(
        "key12",
        "getKey1"  -> tFun(tVar("a"))(tVar("b1")),
        "getKey2"  -> tFun(tVar("a"))(tVar("b2")),
        "getKey3"  -> tFun(tVar("a"))(tVar("b3")),
        "getKey4"  -> tFun(tVar("a"))(tVar("b4")),
        "getKey5"  -> tFun(tVar("a"))(tVar("b5")),
        "getKey6"  -> tFun(tVar("a"))(tVar("b6")),
        "getKey7"  -> tFun(tVar("a"))(tVar("b7")),
        "getKey8"  -> tFun(tVar("a"))(tVar("b8")),
        "getKey9"  -> tFun(tVar("a"))(tVar("b9")),
        "getKey10" -> tFun(tVar("a"))(tVar("b10")),
        "getKey11" -> tFun(tVar("a"))(tVar("b11")),
        "getKey12" -> tFun(tVar("a"))(tVar("b12")),
        "a"        -> tVar("a")
      )(
        key12Type(
          tVar("b1"),
          tVar("b2"),
          tVar("b3"),
          tVar("b4"),
          tVar("b5"),
          tVar("b6"),
          tVar("b7"),
          tVar("b8"),
          tVar("b9"),
          tVar("b10"),
          tVar("b11"),
          tVar("b12")
        )
      ),
      vSpec(
        "key13",
        "getKey1"  -> tFun(tVar("a"))(tVar("b1")),
        "getKey2"  -> tFun(tVar("a"))(tVar("b2")),
        "getKey3"  -> tFun(tVar("a"))(tVar("b3")),
        "getKey4"  -> tFun(tVar("a"))(tVar("b4")),
        "getKey5"  -> tFun(tVar("a"))(tVar("b5")),
        "getKey6"  -> tFun(tVar("a"))(tVar("b6")),
        "getKey7"  -> tFun(tVar("a"))(tVar("b7")),
        "getKey8"  -> tFun(tVar("a"))(tVar("b8")),
        "getKey9"  -> tFun(tVar("a"))(tVar("b9")),
        "getKey10" -> tFun(tVar("a"))(tVar("b10")),
        "getKey11" -> tFun(tVar("a"))(tVar("b11")),
        "getKey12" -> tFun(tVar("a"))(tVar("b12")),
        "getKey13" -> tFun(tVar("a"))(tVar("b13")),
        "a"        -> tVar("a")
      )(
        key13Type(
          tVar("b1"),
          tVar("b2"),
          tVar("b3"),
          tVar("b4"),
          tVar("b5"),
          tVar("b6"),
          tVar("b7"),
          tVar("b8"),
          tVar("b9"),
          tVar("b10"),
          tVar("b11"),
          tVar("b12"),
          tVar("b13")
        )
      ),
      vSpec(
        "key14",
        "getKey1"  -> tFun(tVar("a"))(tVar("b1")),
        "getKey2"  -> tFun(tVar("a"))(tVar("b2")),
        "getKey3"  -> tFun(tVar("a"))(tVar("b3")),
        "getKey4"  -> tFun(tVar("a"))(tVar("b4")),
        "getKey5"  -> tFun(tVar("a"))(tVar("b5")),
        "getKey6"  -> tFun(tVar("a"))(tVar("b6")),
        "getKey7"  -> tFun(tVar("a"))(tVar("b7")),
        "getKey8"  -> tFun(tVar("a"))(tVar("b8")),
        "getKey9"  -> tFun(tVar("a"))(tVar("b9")),
        "getKey10" -> tFun(tVar("a"))(tVar("b10")),
        "getKey11" -> tFun(tVar("a"))(tVar("b11")),
        "getKey12" -> tFun(tVar("a"))(tVar("b12")),
        "getKey13" -> tFun(tVar("a"))(tVar("b13")),
        "getKey14" -> tFun(tVar("a"))(tVar("b14")),
        "a"        -> tVar("a")
      )(
        key14Type(
          tVar("b1"),
          tVar("b2"),
          tVar("b3"),
          tVar("b4"),
          tVar("b5"),
          tVar("b6"),
          tVar("b7"),
          tVar("b8"),
          tVar("b9"),
          tVar("b10"),
          tVar("b11"),
          tVar("b12"),
          tVar("b13"),
          tVar("b14")
        )
      ),
      vSpec(
        "key15",
        "getKey1"  -> tFun(tVar("a"))(tVar("b1")),
        "getKey2"  -> tFun(tVar("a"))(tVar("b2")),
        "getKey3"  -> tFun(tVar("a"))(tVar("b3")),
        "getKey4"  -> tFun(tVar("a"))(tVar("b4")),
        "getKey5"  -> tFun(tVar("a"))(tVar("b5")),
        "getKey6"  -> tFun(tVar("a"))(tVar("b6")),
        "getKey7"  -> tFun(tVar("a"))(tVar("b7")),
        "getKey8"  -> tFun(tVar("a"))(tVar("b8")),
        "getKey9"  -> tFun(tVar("a"))(tVar("b9")),
        "getKey10" -> tFun(tVar("a"))(tVar("b10")),
        "getKey11" -> tFun(tVar("a"))(tVar("b11")),
        "getKey12" -> tFun(tVar("a"))(tVar("b12")),
        "getKey13" -> tFun(tVar("a"))(tVar("b13")),
        "getKey14" -> tFun(tVar("a"))(tVar("b14")),
        "getKey15" -> tFun(tVar("a"))(tVar("b15")),
        "a"        -> tVar("a")
      )(
        key15Type(
          tVar("b1"),
          tVar("b2"),
          tVar("b3"),
          tVar("b4"),
          tVar("b5"),
          tVar("b6"),
          tVar("b7"),
          tVar("b8"),
          tVar("b9"),
          tVar("b10"),
          tVar("b11"),
          tVar("b12"),
          tVar("b13"),
          tVar("b14"),
          tVar("b15")
        )
      ),
      vSpec(
        "key16",
        "getKey1"  -> tFun(tVar("a"))(tVar("b1")),
        "getKey2"  -> tFun(tVar("a"))(tVar("b2")),
        "getKey3"  -> tFun(tVar("a"))(tVar("b3")),
        "getKey4"  -> tFun(tVar("a"))(tVar("b4")),
        "getKey5"  -> tFun(tVar("a"))(tVar("b5")),
        "getKey6"  -> tFun(tVar("a"))(tVar("b6")),
        "getKey7"  -> tFun(tVar("a"))(tVar("b7")),
        "getKey8"  -> tFun(tVar("a"))(tVar("b8")),
        "getKey9"  -> tFun(tVar("a"))(tVar("b9")),
        "getKey10" -> tFun(tVar("a"))(tVar("b10")),
        "getKey11" -> tFun(tVar("a"))(tVar("b11")),
        "getKey12" -> tFun(tVar("a"))(tVar("b12")),
        "getKey13" -> tFun(tVar("a"))(tVar("b13")),
        "getKey14" -> tFun(tVar("a"))(tVar("b14")),
        "getKey15" -> tFun(tVar("a"))(tVar("b15")),
        "getKey16" -> tFun(tVar("a"))(tVar("b16")),
        "a"        -> tVar("a")
      )(
        key16Type(
          tVar("b1"),
          tVar("b2"),
          tVar("b3"),
          tVar("b4"),
          tVar("b5"),
          tVar("b6"),
          tVar("b7"),
          tVar("b8"),
          tVar("b9"),
          tVar("b10"),
          tVar("b11"),
          tVar("b12"),
          tVar("b13"),
          tVar("b14"),
          tVar("b15"),
          tVar("b16")
        )
      )
    )
  )

  def key0Type: UType =
    reference(toFQName(moduleName, "Key0"))

  def key0Type[A](attributes: A): Type[A] =
    reference(attributes, toFQName(moduleName, "Key0"))

  def key2Type(itemType1: UType, itemType2: UType): UType =
    reference(toFQName(moduleName, "Key2"), itemType1, itemType2)

  def key2Type[A](attributes: A)(itemType1: Type[A], itemType2: Type[A]): Type[A] =
    reference(attributes, toFQName(moduleName, "Key2"), itemType1, itemType2)

  def key3Type(itemType1: UType, itemType2: UType, itemType3: UType): UType =
    reference(toFQName(moduleName, "Key3"), itemType1, itemType2, itemType3)

  def key3Type[A](attributes: A)(itemType1: Type[A], itemType2: Type[A], itemType3: Type[A]): Type[A] =
    reference(attributes, toFQName(moduleName, "Key3"), itemType1, itemType2, itemType3)

  def key4Type(itemType1: UType, itemType2: UType, itemType3: UType, itemType4: UType): UType =
    reference(toFQName(moduleName, "Key4"), itemType1, itemType2, itemType3, itemType4)

  def key4Type[A](
      attributes: A
  )(itemType1: Type[A], itemType2: Type[A], itemType3: Type[A], itemType4: Type[A]): Type[A] =
    reference(attributes, toFQName(moduleName, "Key4"), itemType1, itemType2, itemType3, itemType4)

  def key5Type(itemType1: UType, itemType2: UType, itemType3: UType, itemType4: UType, itemType5: UType): UType =
    reference(toFQName(moduleName, "Key5"), itemType1, itemType2, itemType3, itemType4, itemType5)

  def key5Type[A](
      attributes: A
  )(itemType1: Type[A], itemType2: Type[A], itemType3: Type[A], itemType4: Type[A], itemType5: Type[A]): Type[A] =
    reference(attributes, toFQName(moduleName, "Key5"), itemType1, itemType2, itemType3, itemType4, itemType5)

  def key6Type(
      itemType1: UType,
      itemType2: UType,
      itemType3: UType,
      itemType4: UType,
      itemType5: UType,
      itemType6: UType
  ): UType =
    reference(toFQName(moduleName, "Key6"), itemType1, itemType2, itemType3, itemType4, itemType5, itemType6)

  def key6Type[A](attributes: A)(
      itemType1: Type[A],
      itemType2: Type[A],
      itemType3: Type[A],
      itemType4: Type[A],
      itemType5: Type[A],
      itemType6: Type[A]
  ): Type[A] =
    reference(
      attributes,
      toFQName(moduleName, "Key6"),
      itemType1,
      itemType2,
      itemType3,
      itemType4,
      itemType5,
      itemType6
    )

  def key7Type(
      itemType1: UType,
      itemType2: UType,
      itemType3: UType,
      itemType4: UType,
      itemType5: UType,
      itemType6: UType,
      itemType7: UType
  ): UType =
    reference(toFQName(moduleName, "Key7"), itemType1, itemType2, itemType3, itemType4, itemType5, itemType6, itemType7)

  def key7Type[A](attributes: A)(
      itemType1: Type[A],
      itemType2: Type[A],
      itemType3: Type[A],
      itemType4: Type[A],
      itemType5: Type[A],
      itemType6: Type[A],
      itemType7: Type[A]
  ): Type[A] =
    reference(
      attributes,
      toFQName(moduleName, "Key7"),
      itemType1,
      itemType2,
      itemType3,
      itemType4,
      itemType5,
      itemType6,
      itemType7
    )

  def key8Type(
      itemType1: UType,
      itemType2: UType,
      itemType3: UType,
      itemType4: UType,
      itemType5: UType,
      itemType6: UType,
      itemType7: UType,
      itemType8: UType
  ): UType =
    reference(
      toFQName(moduleName, "Key8"),
      itemType1,
      itemType2,
      itemType3,
      itemType4,
      itemType5,
      itemType6,
      itemType7,
      itemType8
    )

  def key8Type[A](attributes: A)(
      itemType1: Type[A],
      itemType2: Type[A],
      itemType3: Type[A],
      itemType4: Type[A],
      itemType5: Type[A],
      itemType6: Type[A],
      itemType7: Type[A],
      itemType8: Type[A]
  ): Type[A] =
    reference(
      attributes,
      toFQName(moduleName, "Key8"),
      itemType1,
      itemType2,
      itemType3,
      itemType4,
      itemType5,
      itemType6,
      itemType7,
      itemType8
    )

  def key9Type(
      itemType1: UType,
      itemType2: UType,
      itemType3: UType,
      itemType4: UType,
      itemType5: UType,
      itemType6: UType,
      itemType7: UType,
      itemType8: UType,
      itemType9: UType
  ): UType =
    reference(
      toFQName(moduleName, "Key9"),
      itemType1,
      itemType2,
      itemType3,
      itemType4,
      itemType5,
      itemType6,
      itemType7,
      itemType8,
      itemType9
    )

  def key9Type[A](attributes: A)(
      itemType1: Type[A],
      itemType2: Type[A],
      itemType3: Type[A],
      itemType4: Type[A],
      itemType5: Type[A],
      itemType6: Type[A],
      itemType7: Type[A],
      itemType8: Type[A],
      itemType9: Type[A]
  ): Type[A] =
    reference(
      attributes,
      toFQName(moduleName, "Key9"),
      itemType1,
      itemType2,
      itemType3,
      itemType4,
      itemType5,
      itemType6,
      itemType7,
      itemType8,
      itemType9
    )

  def key10Type(
      itemType1: UType,
      itemType2: UType,
      itemType3: UType,
      itemType4: UType,
      itemType5: UType,
      itemType6: UType,
      itemType7: UType,
      itemType8: UType,
      itemType9: UType,
      itemType10: UType
  ): UType =
    reference(
      toFQName(moduleName, "Key10"),
      itemType1,
      itemType2,
      itemType3,
      itemType4,
      itemType5,
      itemType6,
      itemType7,
      itemType8,
      itemType9,
      itemType10
    )

  def key10Type[A](attributes: A)(
      itemType1: Type[A],
      itemType2: Type[A],
      itemType3: Type[A],
      itemType4: Type[A],
      itemType5: Type[A],
      itemType6: Type[A],
      itemType7: Type[A],
      itemType8: Type[A],
      itemType9: Type[A],
      itemType10: Type[A]
  ): Type[A] =
    reference(
      attributes,
      toFQName(moduleName, "Key10"),
      itemType1,
      itemType2,
      itemType3,
      itemType4,
      itemType5,
      itemType6,
      itemType7,
      itemType8,
      itemType9,
      itemType10
    )

  def key11Type(
      itemType1: UType,
      itemType2: UType,
      itemType3: UType,
      itemType4: UType,
      itemType5: UType,
      itemType6: UType,
      itemType7: UType,
      itemType8: UType,
      itemType9: UType,
      itemType10: UType,
      itemType11: UType
  ): UType =
    reference(
      toFQName(moduleName, "Key11"),
      itemType1,
      itemType2,
      itemType3,
      itemType4,
      itemType5,
      itemType6,
      itemType7,
      itemType8,
      itemType9,
      itemType10,
      itemType11
    )

  def key11Type[A](attributes: A)(
      itemType1: Type[A],
      itemType2: Type[A],
      itemType3: Type[A],
      itemType4: Type[A],
      itemType5: Type[A],
      itemType6: Type[A],
      itemType7: Type[A],
      itemType8: Type[A],
      itemType9: Type[A],
      itemType10: Type[A],
      itemType11: Type[A]
  ): Type[A] =
    reference(
      attributes,
      toFQName(moduleName, "Key11"),
      itemType1,
      itemType2,
      itemType3,
      itemType4,
      itemType5,
      itemType6,
      itemType7,
      itemType8,
      itemType9,
      itemType10,
      itemType11
    )

  def key12Type(
      itemType1: UType,
      itemType2: UType,
      itemType3: UType,
      itemType4: UType,
      itemType5: UType,
      itemType6: UType,
      itemType7: UType,
      itemType8: UType,
      itemType9: UType,
      itemType10: UType,
      itemType11: UType,
      itemType12: UType
  ): UType =
    reference(
      toFQName(moduleName, "Key12"),
      itemType1,
      itemType2,
      itemType3,
      itemType4,
      itemType5,
      itemType6,
      itemType7,
      itemType8,
      itemType9,
      itemType10,
      itemType11,
      itemType12
    )

  def key12Type[A](attributes: A)(
      itemType1: Type[A],
      itemType2: Type[A],
      itemType3: Type[A],
      itemType4: Type[A],
      itemType5: Type[A],
      itemType6: Type[A],
      itemType7: Type[A],
      itemType8: Type[A],
      itemType9: Type[A],
      itemType10: Type[A],
      itemType11: Type[A],
      itemType12: Type[A]
  ): Type[A] =
    reference(
      attributes,
      toFQName(moduleName, "Key12"),
      itemType1,
      itemType2,
      itemType3,
      itemType4,
      itemType5,
      itemType6,
      itemType7,
      itemType8,
      itemType9,
      itemType10,
      itemType11,
      itemType12
    )

  def key13Type(
      itemType1: UType,
      itemType2: UType,
      itemType3: UType,
      itemType4: UType,
      itemType5: UType,
      itemType6: UType,
      itemType7: UType,
      itemType8: UType,
      itemType9: UType,
      itemType10: UType,
      itemType11: UType,
      itemType12: UType,
      itemType13: UType
  ): UType =
    reference(
      toFQName(moduleName, "Key13"),
      itemType1,
      itemType2,
      itemType3,
      itemType4,
      itemType5,
      itemType6,
      itemType7,
      itemType8,
      itemType9,
      itemType10,
      itemType11,
      itemType12,
      itemType13
    )

  def key13Type[A](attributes: A)(
      itemType1: Type[A],
      itemType2: Type[A],
      itemType3: Type[A],
      itemType4: Type[A],
      itemType5: Type[A],
      itemType6: Type[A],
      itemType7: Type[A],
      itemType8: Type[A],
      itemType9: Type[A],
      itemType10: Type[A],
      itemType11: Type[A],
      itemType12: Type[A],
      itemType13: Type[A]
  ): Type[A] =
    reference(
      attributes,
      toFQName(moduleName, "Key13"),
      itemType1,
      itemType2,
      itemType3,
      itemType4,
      itemType5,
      itemType6,
      itemType7,
      itemType8,
      itemType9,
      itemType10,
      itemType11,
      itemType12,
      itemType13
    )

  def key14Type(
      itemType1: UType,
      itemType2: UType,
      itemType3: UType,
      itemType4: UType,
      itemType5: UType,
      itemType6: UType,
      itemType7: UType,
      itemType8: UType,
      itemType9: UType,
      itemType10: UType,
      itemType11: UType,
      itemType12: UType,
      itemType13: UType,
      itemType14: UType
  ): UType =
    reference(
      toFQName(moduleName, "Key14"),
      itemType1,
      itemType2,
      itemType3,
      itemType4,
      itemType5,
      itemType6,
      itemType7,
      itemType8,
      itemType9,
      itemType10,
      itemType11,
      itemType12,
      itemType13,
      itemType14
    )

  def key14Type[A](attributes: A)(
      itemType1: Type[A],
      itemType2: Type[A],
      itemType3: Type[A],
      itemType4: Type[A],
      itemType5: Type[A],
      itemType6: Type[A],
      itemType7: Type[A],
      itemType8: Type[A],
      itemType9: Type[A],
      itemType10: Type[A],
      itemType11: Type[A],
      itemType12: Type[A],
      itemType13: Type[A],
      itemType14: Type[A]
  ): Type[A] =
    reference(
      attributes,
      toFQName(moduleName, "Key14"),
      itemType1,
      itemType2,
      itemType3,
      itemType4,
      itemType5,
      itemType6,
      itemType7,
      itemType8,
      itemType9,
      itemType10,
      itemType11,
      itemType12,
      itemType13,
      itemType14
    )

  def key15Type(
      itemType1: UType,
      itemType2: UType,
      itemType3: UType,
      itemType4: UType,
      itemType5: UType,
      itemType6: UType,
      itemType7: UType,
      itemType8: UType,
      itemType9: UType,
      itemType10: UType,
      itemType11: UType,
      itemType12: UType,
      itemType13: UType,
      itemType14: UType,
      itemType15: UType
  ): UType =
    reference(
      toFQName(moduleName, "Key15"),
      itemType1,
      itemType2,
      itemType3,
      itemType4,
      itemType5,
      itemType6,
      itemType7,
      itemType8,
      itemType9,
      itemType10,
      itemType11,
      itemType12,
      itemType13,
      itemType14,
      itemType15
    )

  def key15Type[A](attributes: A)(
      itemType1: Type[A],
      itemType2: Type[A],
      itemType3: Type[A],
      itemType4: Type[A],
      itemType5: Type[A],
      itemType6: Type[A],
      itemType7: Type[A],
      itemType8: Type[A],
      itemType9: Type[A],
      itemType10: Type[A],
      itemType11: Type[A],
      itemType12: Type[A],
      itemType13: Type[A],
      itemType14: Type[A],
      itemType15: Type[A]
  ): Type[A] =
    reference(
      attributes,
      toFQName(moduleName, "Key15"),
      itemType1,
      itemType2,
      itemType3,
      itemType4,
      itemType5,
      itemType6,
      itemType7,
      itemType8,
      itemType9,
      itemType10,
      itemType11,
      itemType12,
      itemType13,
      itemType14,
      itemType15
    )

  def key16Type(
      itemType1: UType,
      itemType2: UType,
      itemType3: UType,
      itemType4: UType,
      itemType5: UType,
      itemType6: UType,
      itemType7: UType,
      itemType8: UType,
      itemType9: UType,
      itemType10: UType,
      itemType11: UType,
      itemType12: UType,
      itemType13: UType,
      itemType14: UType,
      itemType15: UType,
      itemType16: UType
  ): UType =
    reference(
      toFQName(moduleName, "Key16"),
      itemType1,
      itemType2,
      itemType3,
      itemType4,
      itemType5,
      itemType6,
      itemType7,
      itemType8,
      itemType9,
      itemType10,
      itemType11,
      itemType12,
      itemType13,
      itemType14,
      itemType15,
      itemType16
    )

  def key16Type[A](attributes: A)(
      itemType1: Type[A],
      itemType2: Type[A],
      itemType3: Type[A],
      itemType4: Type[A],
      itemType5: Type[A],
      itemType6: Type[A],
      itemType7: Type[A],
      itemType8: Type[A],
      itemType9: Type[A],
      itemType10: Type[A],
      itemType11: Type[A],
      itemType12: Type[A],
      itemType13: Type[A],
      itemType14: Type[A],
      itemType15: Type[A],
      itemType16: Type[A]
  ): Type[A] =
    reference(
      attributes,
      toFQName(moduleName, "Key16"),
      itemType1,
      itemType2,
      itemType3,
      itemType4,
      itemType5,
      itemType6,
      itemType7,
      itemType8,
      itemType9,
      itemType10,
      itemType11,
      itemType12,
      itemType13,
      itemType14,
      itemType15,
      itemType16
    )

}
