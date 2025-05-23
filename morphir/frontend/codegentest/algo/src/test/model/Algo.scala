package test.model

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

  def threeTypes(a: String, b: Float, c: Int): Int = c

  def useThreeTypes(a: String, b: Float, c: Int, d: Int) =
    threeTypes(a, b, c + d) * (divide(d, c) + identity(c))