package zio.morphir.ir

/**
 * In functional programming data and logic are treated the same way and we refer to both as values. This module
 * provides the building blocks for those values (data and logic) in the Morphir IR.
 *
 * If you use Elm as your frontend language for Morphir then you should think about all the logic and constant values
 * that you can put in the body of a function. Here are a few examples:
 *
 * {{{
 * myThreshold =
 *     1000
 *
 * min a b =
 *   if a < b then
 *     a
 *   else b
 *
 * addTwo a = a + 2
 * }}}
 *
 * All the above are values: the first one is just data, the second one is logic and the last one has both logic and
 * data. In either case each value is represented by a [`Value`](#Value) expression. This is a recursive data structure
 * with various node types representing each possible language construct. You can check out the documentation for values
 * below to find more details. Here are the Morphir IR snippets for the above values as a quick reference:
 *
 * {{{
 * myThreshold = Literal () (WholeNumberLiteral 1000)
 *
 * min a b = IfThenElse () (Apply () (Apply () (Reference () (fqn "Morphir.SDK" "Basics" "lessThan")) (Variable () [ "a"
 * ]) ) (Variable () [ "b" ]) ) (Variable () [ "a" ]) (Variable () [ "b" ])
 *
 * addTwo a = Apply () (Apply () (Reference () (fqn "Morphir.SDK" "Basics" "add")) (Variable () [ "a" ]) ) (Literal ()
 * (WholeNumberLiteral 2))
 * }}}
 */
object Value extends value.recursive.ValueModule
