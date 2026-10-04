package examples

import scala.math.{ abs, max, min }

enum Tier:
  case Plus, Vip

enum Reward:
  case Bonus(points: Int)
  case Grade(mark: Char)

enum Palette:
  case Named(label: String, intensity: Int, priority: Int)
  case Rgb(red: Int, green: Int, blue: Int)

case class DualBox[A, B](right: B, left: A)

case class Person(age: Option[Long], bonus: Long):
  def normalizedBonus: Long =
    if bonus > 0L then bonus + 1L
    else bonus

  def isAtLeast(threshold: Long): Boolean =
    bonus >= threshold

  def adjustedBonus(threshold: Long): Long =
    if bonus + 1L > threshold then bonus + 1L
    else threshold

  def boundedBonus(lowerBound: Long, upperBound: Long): Long =
    if bonus < lowerBound then lowerBound
    else if bonus > upperBound then upperBound
    else bonus

case class Envelope[A, B](box: DualBox[A, B], fee: Long)

object CurrentSupportedExample:
  def greeting: String =
    "hello"

  def adjustedScore(input: Envelope[Person, Tier], fallbackAge: Long): (Long, Tier) =
    val age =
      input.box.left.age match
        case Some(value) => value
        case None        => fallbackAge

    val tierBonus =
      input.box.right match
        case Tier.Vip  => 10L
        case Tier.Plus => 5L

    val withBonus = age + input.box.left.normalizedBonus + tierBonus

    val finalScore =
      if input.fee > 0L then withBonus - input.fee
      else withBonus

    val finalTier =
      if finalScore > 20L then Tier.Vip
      else input.box.right

    (finalScore, finalTier)

  def normalizedHistory(input: Option[List[Long]]): List[Long] =
    input match
      case Some(values) => values
      case None         => Nil

  def projectTier(result: (Long, Tier)): Tier =
    result match
      case (_, tier) => tier

  def rewardCode(reward: Reward): Char =
    reward match
      case Reward.Bonus(_)    => 'B'
      case Reward.Grade(mark) => mark

  def floatBand(value: Float): Int =
    value match
      case 1.5f => 10
      case 2.5f => 20
      case _    => 0

  def defaultThresholds: List[Int] =
    List(10, 20)

  def thresholdGroups: List[List[Int]] =
    List(List(10, 20), List(30))

  def thresholdCount(values: List[Int]): Int =
    values.length

  def incrementThresholds(values: List[Int]): List[Int] =
    values.map(value => value + 1)

  def positiveThresholds(values: List[Int]): List[Int] =
    values.filter(value => value > 0)

  def expandedThresholds(values: List[Int]): List[Int] =
    values.flatMap(value => List(value, value + 1))

  def thresholdTotal(values: List[Int]): Int =
    values.foldLeft(0)((total, value) => total + value)

  def boundedMagnitude(value: Int, lower: Int, upper: Int): Int =
    max(lower, min(abs(value), upper))

  def floatAverage(total: Int, count: Int): Double =
    total.toDouble / count.toDouble

  def truncatedBand(value: Float): Int =
    value.toInt

  def collectedThresholds(values: List[Option[Int]]): List[Int] =
    values.collect { case Some(value) =>
      value + 1
    }

  def normalizedSeqThresholds(values: Seq[Int]): Seq[Int] =
    values.flatMap(value => Seq(value, value + 1))

  def thresholdMap: Map[String, Int] =
    Map("low" -> 10, "high" -> 20)

  def thresholdValue(name: String, values: Map[String, Int]): Option[Int] =
    values.get(name)

  def incrementedWithFor(values: List[Int]): List[Int] =
    for value <- values
    yield value + 1

  def summedPairs(values: List[(Int, Int)]): List[Int] =
    values.map(pair =>
      pair match
        case (left, right) => left + right
    )

  def keepsReward(person: Person, reward: Reward): Reward =
    if person.boundedBonus(10L, 20L) > 10L then reward
    else Reward.Bonus(0)

  def sumPair(pair: (Int, Int)): Int =
    val (left, right) = pair
    left + right

  def firstOfTriple(triple: (Int, String, Boolean)): Int =
    triple match
      case (first, _, _) => first

  def firstOfQuadruple(quadruple: (Int, String, Boolean, Float)): Int =
    quadruple match
      case (first, _, _, _) => first

  def sumQuadruple(quadruple: (Int, Int, Int, Int)): Int =
    val (w, x, y, z) = quadruple
    w + x + y + z

  def paletteIntensity(palette: Palette): Int =
    palette match
      case Palette.Named(_, intensity, _) => intensity
      case Palette.Rgb(red, green, blue)  => red + green + blue
