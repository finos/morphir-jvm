package morphir.ir.codec

import morphir.ir.codec.`type`._

object TypeCodec extends AllTypeExpressionsCodec
trait TypeCodec
    extends VariableCodec
    with ReferenceCodec
    with TupleCodec
    with RecordCodec
    with ExtensibleRecordCodec
    with FunctionCodec
    with UnitCodec
    with TypeCoproductCodec {}
