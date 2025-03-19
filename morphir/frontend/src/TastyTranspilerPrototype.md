# Overview

The purpose of this module is to convert business logic captured in [Scala 3 TASTy files](https://docs.scala-lang.org/scala3/guides/tasty-overview.html) to the Morphir IR JSON format.  
Then the Morphir IR file can be used to visualize the business logic, potentially with actual production data.

The following Scala structures are being considered for initial support:
- case classes for data modeling and exchange
- objects with [pure functions](https://en.wikipedia.org/wiki/Pure_function) for business logic

# Technical Glimpse

A [scala.tasty.inspector.Inspector](morphir/codegen/tasty/TastyToMorphir.scala) class is written to traverse the Typed Abstract Syntax Tree (TASTy).  
As tree traversal goes, Morphir IR model objects are created and assembled. Once the final model is ready, it is serialized to JSON.

More can be found about the TASTy Inspector in the corresponding [Scala 3 metaprogramming article](https://docs.scala-lang.org/scala3/reference/metaprogramming/tasty-inspect.html).

# Current State

Supported types:
- Int
- Float

Supported operators:
- addition (+)
- subtraction (-)
- multiplication (*)
- division (/)

While this module is in its infancy, the TASTy file generated from the following simple code snippet can be successfully converted to Morphir IR JSON:
```scala
object Algo:
  def plus(a: Int, b: Int, c: Int): Int = a + b + c

  def identity(a: Int): Int = a

  def minus(a: Int, b: Int): Int = a - b

  def multiply(a: Int, b: Int): Int = a * b

  def divide(a: Int, b: Int): Int = a / b

  def divideFloat(a: Float, b: Float): Float = a / b

  def complex(a: Int, b: Int, c: Int, d: Int): Int = a + b - c * d

  def complex2(a: Int, b: Int, c: Int, d: Int): Int = a - (b + c) * d

  // Current limitation: 2f must be used instead of 2 to force float literal instead of integer
  def circlePerimeter(radius: Float): Float = 2f * 3.14f * radius
```

Possible next steps:
- Subroutine calling
- Scala BigDecimal support
- Case class support
- Branching
- Comparison operators
