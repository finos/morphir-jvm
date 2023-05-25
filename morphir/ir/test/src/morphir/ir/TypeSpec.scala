package morphir.ir

import io.circe.{Json, ParsingFailure, parser}
import morphir.ir.FQName.FQName
import morphir.ir.Name.Name
import morphir.ir._type.Codec
import zio.test.Assertion._
import zio.test._

object TypeSpec extends DefaultRunnableSpec{
    val fqn: FQName = FQName.fromString("Morphir.SDK:Int:Int")(":")
    val sdkIntReferenceType: Type.Type[scala.Unit] = Type.Reference({}, fqn, List())
    val customTypeDefName: List[Name] = List(Name.fromString("CustomerType"))
    val ctorArgName: Name = Name.fromString("ctorArg1")
    val ctorArgs: Type.ConstructorArgs[scala.Unit]= List((ctorArgName, sdkIntReferenceType))
    val emptyCtorArgs = List((Name.fromString(""), Type.Type.Unit))
    val constructorArgsFromIRJson: Either[ParsingFailure, Json] = parser.parse("""[[
                                                   |	["ctor", "arg","1"],
                                                   | ["Reference",{},[[["morphir"],["s","d","k"]],[["int"]],["int"]],[]]
                                                   |]]""".stripMargin)

    override def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] = suite("Serialization / De-serialization of Types")(
        suite("Encoding / Decoding ConstructorArgs")(
            test("Encoding"){
                val encodedConstructorArgs = Codec.encodeConstructorArgs(io.circe.Encoder.encodeUnit)(ctorArgs)
                val expectedJson = constructorArgsFromIRJson.getOrElse(Json.Null)

                assert(encodedConstructorArgs)(equalTo(expectedJson))
            },

            test("Decoding") {
                val decodedConstructorArgs = Codec.decodeConstructorArgs(io.circe.Decoder.decodeUnit)(constructorArgsFromIRJson.getOrElse(Json.Null).hcursor)
                val expectedConstructorArgs = List((ctorArgName, sdkIntReferenceType))

                assert(decodedConstructorArgs.getOrElse(emptyCtorArgs))(equalTo(expectedConstructorArgs))
            }
        )
    )
}
