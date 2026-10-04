package a.b

import a.b.c.AddOne.addOne

object UseNestedAddOne:
  def addTwo(value: Int): Int =
    addOne(value) + 1
