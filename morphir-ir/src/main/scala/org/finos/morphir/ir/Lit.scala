package org.finos.morphir.ir

enum Lit:
  case Bool(value: Boolean)
  case Char(value: scala.Char)
  case Str(value: String)
  case Int(value: scala.Int) //TODO: Maybe BigInt
  case Float(value: scala.Double)
