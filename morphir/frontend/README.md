# Overview

The purpose of this module is to convert business logic captured in Scala 3 TASTy files to the Morphir IR JSON format. In other words this is a Scala 3 to Morphir IR transpiler.  
This is currently in incubation phase, with limited support for Scala 3 language features.

# Technical Glimpse

A [scala.tasty.inspector.Inspector](src/morphir/codegen/tasty/TastyToMorphir.scala) class is written to traverse the Typed Abstract Syntax Tree (TASTy).  
As tree traversal goes, a morphir.ir.FormatVersion is being assembled.

Learn more about the TASTy Inspector in the corresponding [Scala 3 metaprogramming article](https://docs.scala-lang.org/scala3/reference/metaprogramming/tasty-inspect.html).

# Module structure

Building this module consists of the following steps:
1. Compile the transpiler (src folder).
1. Run unit tests on the transpiler code (test folder). Arguably this can be removed, as the codegentest should provide better coverage.
1. Compile test algorithms to TASTy files (codegentest/algo folder).
1. Generate code and compile it (see the codegentest/generated/generatedSources section in the build.mill file):
   1. Generate a Morphir IR JSON file from the above TASTy files.
   1. Generate Scala code from the Morphir IR JSON file using the morphir-elm CLI
   1. Compile the generated Scala code
1. Run pre-defined unit tests on the generated code (codegentest/generated/test folder) 

# Current State

Supported types:
- Int
- Float

Supported operators:
- addition (+)
- subtraction (-)
- multiplication (*)
- division (/)
- integer division (/)

Supported language structures:
- subroutine calling

See the [Algo.scala](codegentest/algo/src/test/model/Algo.scala) for working examples.

# Planned next steps

- Branch support (if/else for starter, with operators such as !=, <, >)
- Scala BigDecimal support
- Case class support
