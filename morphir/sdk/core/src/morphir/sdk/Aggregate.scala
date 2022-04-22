package morphir.sdk

import scala.collection.immutable.HashMap

object Aggregate {

  sealed trait Operator[+A]
  object Operator {

    final case object Count extends Operator[Nothing]

    final case class Sum[A](getValue: A => Double) extends Operator[A]

    final case class Avg[A](getValue: A => Double) extends Operator[A]

    final case class Min[A](getValue: A => Double) extends Operator[A]

    final case class Max[A](getValue: A => Double) extends Operator[A]

    final case class WAvg[A](getWeight: A => Double, getValue: A => Double) extends Operator[A]

  }

  def count[A]: Aggregation[A, Unit] =
    operatorToAggregation(Operator.Count)

  def sumOf[A](f: A => Double): Aggregation[A, Unit] =
    operatorToAggregation(Operator.Sum(f))

  def averageOf[A](f: A => Double): Aggregation[A, Unit] =
    operatorToAggregation(Operator.Avg(f))

  def minimumOf[A](f: A => Double): Aggregation[A, Unit] =
    operatorToAggregation(Operator.Min(f))

  def maximumOf[A](f: A => Double): Aggregation[A, Unit] =
    operatorToAggregation(Operator.Max(f))

  def weightedAverageOf[A](getWeight: A => Double, getValue: A => Double): Aggregation[A, Unit] =
    operatorToAggregation(Operator.WAvg(getWeight, getValue))

  case class Aggregation[A, K](key: A => K, filter: A => Boolean, operator: Operator[A])

  def operatorToAggregation[A](op: Operator[A]): Aggregation[A, Unit] =
    Aggregation(_ => (), _ => true, op)

  def byKey[A, K](k: A => K)(agg: Aggregation[A, _]): Aggregation[A, K] =
    agg.copy(key = k)

  def withFilter[A, K](f: A => Basics.Bool)(agg: Aggregation[A, K]): Aggregation[A, K] =
    agg.copy(filter = f)

  def aggregateMap[A, B, Key1](
    agg1: Aggregation[A, Key1]
  )(f: Double => A => B)(list: List[A]): List[B] = {
    val aggregated1: Map[Key1, Double] =
      aggregateHelp(agg1.key, agg1.operator, list.filter(agg1.filter))
    for (a <- list)
      yield f(aggregated1.getOrElse(agg1.key(a), 0))(a)
  }

  def aggregateMap2[A, B, Key1, Key2](
    agg1: Aggregation[A, Key1]
  )(agg2: Aggregation[A, Key2])(f: Double => Double => A => B)(list: List[A]): List[B] = {
    val aggregated1: Map[Key1, Double] =
      aggregateHelp(agg1.key, agg1.operator, list.filter(agg1.filter))
    val aggregated2: Map[Key2, Double] =
      aggregateHelp(agg2.key, agg2.operator, list.filter(agg2.filter))
    for (a <- list)
      yield f(aggregated1.getOrElse(agg1.key(a), 0))(aggregated2.getOrElse(agg2.key(a), 0))(a)
  }

  def aggregateMap3[A, B, Key1, Key2, Key3](
    agg1: Aggregation[A, Key1]
  )(
    agg2: Aggregation[A, Key2]
  )(agg3: Aggregation[A, Key3])(f: Double => Double => Double => A => B)(list: List[A]): List[B] = {
    val aggregated1: Map[Key1, Double] =
      aggregateHelp(agg1.key, agg1.operator, list.filter(agg1.filter))
    val aggregated2: Map[Key2, Double] =
      aggregateHelp(agg2.key, agg2.operator, list.filter(agg2.filter))
    val aggregated3: Map[Key3, Double] =
      aggregateHelp(agg3.key, agg3.operator, list.filter(agg3.filter))
    for (a <- list)
      yield f(aggregated1.getOrElse(agg1.key(a), 0))(aggregated2.getOrElse(agg2.key(a), 0))(
        aggregated3.getOrElse(agg3.key(a), 0)
      )(a)
  }

  def aggregateHelp[A, K](getKey: A => K, op: Operator[A], list: List[A]): Map[K, Double] = {
    def aggregate(getValue: A => Double, o: (Double, Double) => Double, sourceList: List[A]): Map[K, Double] =
      sourceList.foldLeft(HashMap[K, Double]()) { (soFar: HashMap[K, Double], nextA: A) =>
        val key = getKey(nextA)
        soFar.get(key) match {
          case Some(value) =>
            soFar.updated(key, o(value, getValue(nextA)))
          case None =>
            soFar.updated(key, getValue(nextA))
        }
      }

    def combine(f: (Double, Double) => Double, dictA: Map[K, Double], dictB: Map[K, Double]): Map[K, Double] =
      dictA.map { case (key, a) =>
        (key, f(a, dictB.getOrElse(key, 0)))
      }

    def sum(getValue: A => Double, sourceList: List[A]): Map[K, Double] =
      aggregate(getValue, _ + _, sourceList)

    op match {
      case Operator.Count =>
        sum(_ => 1.0, list)
      case Operator.Sum(getValue) =>
        sum(getValue.asInstanceOf[A => Double], list)
      case Operator.Avg(getValue) =>
        combine(_ / _, sum(getValue.asInstanceOf[A => Double], list), sum(_ => 1.0, list))
      case Operator.Min(getValue) =>
        aggregate(getValue.asInstanceOf[A => Double], Math.min, list)
      case Operator.Max(getValue) =>
        aggregate(getValue.asInstanceOf[A => Double], Math.max, list)
      case Operator.WAvg(getWeight, getValue) =>
        combine(
          _ / _,
          sum(a => getWeight.asInstanceOf[A => Double](a) * getValue.asInstanceOf[A => Double](a), list),
          sum(getWeight.asInstanceOf[A => Double], list)
        )
    }
  }

}
