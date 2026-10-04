# TASTy to Morphir IR

This project converts Scala 3 TASTy (`.tasty`) files into Morphir IR JSON (distribution format `3`).

It uses the Scala 3 `scala3-tasty-inspector` API to read compiled Scala trees and emits a `VersionedDistribution` JSON suitable for Morphir-based analysis.

## Related projects

- Morphir JVM: <https://github.com/finos/morphir-jvm>
- Morphir Elm CLI: <https://github.com/finos/morphir-elm>
- Scala 3 TASTy Inspector docs: <https://docs.scala-lang.org/scala3/reference/metaprogramming/tasty-inspect.html>

## Requirements

- JDK `11` or `17` (use `17` locally)
- checked-in Mill launcher
- Scala `3.3.6` (managed by Mill)
- npm-locked Morphir Elm `2.96.1` (`npm ci` at the repository root)

## Build

```bash
./mill morphir.frontend.compile
```

## Run

Main entrypoint:

- `morphir.codegen.tasty.tastyToMorphirIR`

Examples:

```bash
# Convert one .tasty file
./mill morphir.frontend.runMain morphir.codegen.tasty.tastyToMorphirIR /tmp/output.json /path/to/File.tasty

# Convert multiple related .tasty files together
./mill morphir.frontend.runMain morphir.codegen.tasty.tastyToMorphirIR /tmp/output.json /path/to/One.tasty /path/to/Two.tasty
```

Arguments:

1. Output path for the generated Morphir IR JSON
2. One or more compiled Scala 3 `.tasty` files

## Examples

- `examples/CurrentSupportedExample.scala` shows the most complex concise Scala shape currently supported end-to-end.
- Keep this example updated as translation support expands so it remains the quick reference for current capabilities.

## Current conversion behavior

### Supported types

- `Boolean`
- `Byte` (mapped to Morphir `int`)
- `Int`
- `Short` (mapped to Morphir `int`)
- `Long` (currently mapped to Morphir `int`)
- `Float`
- `Double` (mapped to Morphir `float`)
- `Char`
- `String`
- `BigDecimal`
- `Unit`
- `Option[T]`
- `List[T]` for the current narrow empty-list, direct `List(...)` literal, nested-list literal, direct `length`, direct single-lambda `map` / `filter` / `flatMap`, narrow `foldLeft`, and narrow `collect` slices
- `Seq[T]` for the current normalization slice covering direct `Seq(...)` literals plus direct `map` / `flatMap` / `foldLeft` onto the proven Morphir list path
- `Map[K, V]` for the current narrow immutable type/literal slice covering direct `Map(...)` literals lowered from `key -> value` pairs
- narrow Scala `enum` custom types, including flat direct constructor payloads up to three arguments
- Scala tuples, including flat destructuring up to four elements
- Scala `case class` data fields, including generic multi-parameter and nested record references

### Supported expressions

- literals, including `Boolean`, `Long`, `Char`, and direct `String` literals
- `Unit` values via `()`
- arithmetic operators: `+`, `-`, `*`, `/`
- boolean operators: `&&`, `||`
- equality operators: `==`, `!=`
- comparison operators: `<`, `<=`, `>`, `>=`
- unary operators: logical negation `!` and numeric negation `-value`
- numeric helpers: `abs`, `min`, `max`, `toDouble`/`toFloat` from `Int`, and `toInt` from `Float`/`Double`
- function application
- `if / else`
- pattern matching for `Option` constructors and supported scalar literal patterns, including `Float`
- constructor references, direct constructor application, and direct constructor pattern matches for the current narrow `enum` slice, including flat three-argument constructors
- tuple literals and tuple-typed pass-through values
- narrow tuple destructuring in `match` expressions for tuple element capture, including flat 4-tuples
- narrow tuple destructuring in local `val` bindings for direct tuple element capture, including flat 4-tuples
- local `val` bindings and block expressions
- case-class field access, including nested record access
- narrow case-class methods defined directly on case classes, including the current two-explicit-parameter slice and richer supported `if`-based method bodies
- empty list values via `List()` and `Nil`
- populated list values via `List(...)`
- nested list literals such as `List(List(1, 2), List(3))`
- direct `List.length`
- direct `List.map` with a single-argument lambda whose body stays within the supported expression surface
- direct `List.filter` with a single-argument lambda whose predicate stays within the supported expression surface
- direct `List.flatMap` with a single-argument lambda whose body returns another supported `List`
- narrow `List.foldLeft` with a two-parameter lambda whose body stays within the supported expression surface
- narrow `List.collect` with a single-case lambda `match` body that can be normalized to a `Maybe`-returning transform
- normalized immutable `Seq(...)` literals
- normalized immutable `Seq.map` / `Seq.flatMap` / `Seq.foldLeft` over the current supported lambda-expression surface
- narrow immutable `Map(...)` literals built from direct `key -> value` pairs
- narrow immutable `Map[K, V]` pass-through signatures
- narrow immutable `Map.get` lookups that return `Option` / Morphir `Maybe`
- pure list `for`-`yield` traversal shapes that lower to the already-supported `map` / `flatMap` surface
- pattern-aware list transforms via explicit `match` expressions inside supported list lambdas, starting with tuple matches

### Mapping notes

- `Int /` maps to Morphir `integerDivide`
- `BigDecimal` arithmetic and comparison map to Morphir decimal SDK functions
- Scala `BigDecimal /` maps to `morphir.SDK.decimal.div.unsafe`
- Scala `Long` currently maps to Morphir `int`
- Scala `Byte` currently maps to Morphir `int`
- Scala `Short` currently maps to Morphir `int`
- Scala `Double` maps to Morphir `float`
- Scala `abs` maps to Morphir `morphir.SDK.basics.abs`
- Scala `min` maps to Morphir `morphir.SDK.basics.min`
- Scala `max` maps to Morphir `morphir.SDK.basics.max`
- Scala `Int.toDouble` / `Int.toFloat` map to Morphir `morphir.SDK.basics.toFloat`
- Scala `Float.toInt` / `Double.toInt` map to Morphir `morphir.SDK.basics.truncate`
- Scala `Char` maps to Morphir `char`
- Scala `Unit` maps to Morphir unit
- Scala `List[T]` maps to Morphir `morphir.SDK.list.list[T]`
- Scala `List.length` maps to Morphir `morphir.SDK.list.length`
- Scala `List.map` maps to Morphir `morphir.SDK.list.map`
- Scala `List.filter` maps to Morphir `morphir.SDK.list.filter`
- Scala `List.flatMap` maps to Morphir `morphir.SDK.list.concatMap`
- Scala `List.foldLeft` maps to Morphir `morphir.SDK.list.foldl`
- Scala `List.collect` maps to Morphir `morphir.SDK.list.filterMap` for the current single-case partial-function slice
- narrow immutable Scala `Seq[T]` currently normalizes to the same Morphir `morphir.SDK.list.list[T]` path as `List[T]`
- narrow immutable Scala `Map[K, V]` currently maps to Morphir `morphir.SDK.dict.dict[K, V]`
- narrow immutable Scala `Map(...)` literals currently map to Morphir `morphir.SDK.dict.fromList`
- narrow immutable Scala `Map.get` currently maps to Morphir `morphir.SDK.dict.get`
- Scala `TupleN` maps to Morphir tuple types and tuple values
- Scala case classes are emitted as Morphir `type alias` records
- narrow case-class methods are emitted as module values with an explicit record receiver input, including the current two-explicit-parameter slice
- narrow Scala `enum` families are emitted as Morphir custom types
- generic case-class fields preserve declared type-parameter order and substitute concrete nested type arguments during field access

## Parity target catalog

The broader parity target is now tracked explicitly in `test/src/morphir/codegen/tasty/ParityCatalog.scala`.

That catalog anchors future work around three target layers:

- Morphir language surfaces such as modules, aliases, records, custom types, literals, functions, branching, tuples, and patterns
- Elm-core-backed SDK surfaces that Morphir maps into its SDK, including basics/bool, numeric families, list, maybe, result, string/char, and function helpers
- Morphir-specific SDK modules such as `Decimal`, `Dict`, `Aggregate`, `Key`, `Rule`, `Validate`, `UUID`, `Instant`, `LocalDate`, `LocalTime`, and `Json.*`

Each catalog entry now also records:

- one or more direct Scala source forms that should map to that Morphir surface
- any important Scala syntactic sugars that should eventually lower to that same Morphir IR
- the current coverage state (`supported`, `partial`, `planned`, or explicitly excluded)
- a concrete gap summary that explains what still blocks full parity for that surface

It also records the explicit Elm effect modules that remain out of scope because they do not have a pure Morphir parity target:

- `Debug`
- `Platform`
- `Process`
- `Task`

### Multiple input `.tasty` files

The converter can merge multiple related `.tasty` files into one Morphir distribution.

For inputs from related Scala namespaces such as:

- `a.b`
- `a.b.c`

the converter:

- computes the longest common package prefix as the output Morphir package
- moves the remaining namespace suffix into module paths
- rebases user-defined type and value references to that common package root

Unrelated package roots still fail fast.

## Upcoming feature roadmap

This is the current ordered plan for the next **10** parity slices.

The order below is now derived from the test-side parity gap matrix in `ParityCatalog.scala` rather than being maintained as a standalone wishlist.

Keep this section updated as the roadmap changes.

1. **Basics and Bool helpers**
     Finish the remaining boolean and basics-oriented helper surface around the already-supported literal and branching core.
2. **Comparable and equality parity**
     Complete equality and ordering semantics across the supported scalar families.
3. **Number helper parity**
     Extend today’s arithmetic coverage into the broader Morphir number helper surface.
4. **Literal breadth**
     Fill the remaining basic literal and literal-adjacent gaps that other slices depend on.
5. **Functions and lambdas breadth**
     Broaden lambda, helper-function, and function-value parity beyond the current narrow forms.
6. **Let / if / case completeness**
     Extend local-binding and branching support toward fuller Elm-style expression parity.
7. **Tuple and record-access breadth**
     Expand tuple-consuming and record-access-driven expression shapes.
8. **Record parity**
     Move from narrow case-class alias support toward fuller record construction and update parity.
9. **Custom-type parity**
     Extend the current enum/custom-type surface to broader ADT representations and constructor usage.
10. **Pattern completeness**
     Broaden pattern matching so later collection and sugar slices can reuse a stronger core matcher.

## Test suite

The repository has a fixture-driven ZIO Test suite under `test/src/morphir/codegen/tasty/`.

The baseline is always the JSON generated from equivalent Elm source using `morphir-elm make -f`.

Main suites:

- `ParityCatalogSpec` (12 catalog tests)
- `FrontendIntegrationSpec` (example conversion and rejection behavior)
- `SupportedFunctionEquivalenceSpec`
- `CaseClassEquivalenceSpec`
- `CaseClassFieldAccessEquivalenceSpec`
- `MixedNamespaceMultiTastySpec`

### Test workflow

```bash
./mill morphir.frontend.test

./mill morphir.frontend.test.testOnly morphir.codegen.tasty.SupportedFunctionEquivalenceSpec
./mill morphir.frontend.test.testOnly morphir.codegen.tasty.CaseClassEquivalenceSpec
./mill morphir.frontend.test.testOnly morphir.codegen.tasty.CaseClassFieldAccessEquivalenceSpec
./mill morphir.frontend.test.testOnly morphir.codegen.tasty.MixedNamespaceMultiTastySpec
```

Both `test` and `testOnly` automatically:

1. compile the Scala fixtures in `test-fixtures/scala/`
2. copy Elm inputs and generate baseline IR under `out/` (cached until inputs change)
3. compile the frontend-local examples

## Test fixtures

Fixture inputs live in:

- `test-fixtures/scala/` for Scala sources compiled to `.tasty`
- `test-fixtures/elm/` for Elm projects compiled to `morphir-ir.json`

The 102 equivalence cases compare full generated JSON distributions directly, so Scala and Elm namespaces are intentionally aligned. All 116 migrated Scala files and 102 Elm projects are retained. Thirteen targeted/catalog tests cover decimal division and the parity roadmap; four integration/rejection tests cover the example and unsupported inputs.

## Repository structure

- `src/morphir/codegen/tasty/TastyToMorphir.scala` - entrypoint and multi-file merge logic
- `src/morphir/codegen/tasty/TreeMorph.scala` - package-level conversion to Morphir distributions
- `src/morphir/codegen/tasty/TypeDefMorph.scala` - object and case-class module extraction
- `src/morphir/codegen/tasty/DefDefMorph.scala` - method definition conversion
- `src/morphir/codegen/tasty/ApplyMorph.scala` - function application conversion
- `src/morphir/codegen/tasty/IdentMorph.scala` - identifier and FQName conversion
- `src/morphir/codegen/tasty/SelectMorph.scala` - selection and field-access conversion
- `src/morphir/codegen/tasty/IfMorph.scala` - `if / else` conversion
- `src/morphir/codegen/tasty/TreeResolver.scala` - shared type resolution
- `src/morphir/codegen/tasty/StandardTypes.scala` - Scala-to-Morphir type mappings
- `src/morphir/codegen/tasty/StandardFunctions.scala` - Scala operator/function mappings

## Limitations

- support is intentionally narrow and fail-fast
- generic case classes are supported for the current narrow slice, including multi-parameter data-only records and nested record references
- user-defined ADTs are currently limited to Scala `enum` cases with the current direct-constructor slice up to three constructor arguments
- additional literal widening currently covers `Long`, `Char`, direct `String` literals, and `Float` literal patterns; other scalar literal expansions remain unsupported
- collection support is currently limited to `List[T]` types plus direct `List(...)` literals, nested-list literals, empty-list values (`List()` and `Nil`), direct `List.length`, narrow direct-lambda `List.map` / `List.filter` / `List.flatMap` slices, a narrow `List.foldLeft` slice, a narrow single-case `List.collect` slice, a narrow immutable `Seq[T]` normalization slice for literals, `map`, `flatMap`, and `foldLeft`, and a narrow immutable `Map[K, V]` slice for direct `Map(...)` literals and pass-through signatures
- case-class methods are currently limited to direct methods on the case class plus at most two explicit parameters, with supported bodies staying within the current expression surface
- tuple destructuring is currently limited to narrow flat tuple-match and local-`val` slices up to the current 4-tuple coverage; broader tuple patterns are still unsupported
- many Scala constructs are still unsupported, including broader ADT families, collection operations, and richer tuple or case-method shapes

## Cleanup

```bash
./mill clean morphir.frontend
```

This removes frontend build output, including generated fixture baselines. Fixture inputs remain in the source tree.

The fixture and example modules are not published. Generated JSON, hashes, and compiled fixtures stay under `out/`. Both `test` and `testOnly` prepare fixtures via test JVM options. Inspector conversions are serialized and temporary JSON outputs are deleted. Run frontend formatting with `./mill morphir.frontend.__.checkFormat`.

This frontend incorporates the converter, fixtures, and documentation from the `tasty-2-morphir` working tree. Planned parity-catalog surfaces remain future work.
