package morphir.flowz

trait StepArities {

  final def mapN[SIn, SA, SB, Msg, R, E, A, B, SOut, Result, MA, MB](
    behaviorA: Step[SIn, SA, Msg, R, E, A],
    behaviorB: Step[SIn, SB, Msg, R, E, B]
  )(
    f: (BehaviorSuccess[SA, A], BehaviorSuccess[SB, B]) => BehaviorSuccess[SOut, Result]
  ): Step[SIn, SOut, Msg, R, E, Result] =
    behaviorA.zipWith(behaviorB)(f)

  final def mapParN[SIn, SA, SB, Msg, R, E, A, B, SOut, Result](
    behaviorA: Step[SIn, SA, Msg, R, E, A],
    behaviorB: Step[SIn, SB, Msg, R, E, B]
  )(
    f: (BehaviorSuccess[SA, A], BehaviorSuccess[SB, B]) => BehaviorSuccess[SOut, Result]
  ): Step[SIn, SOut, Msg, R, E, Result] =
    behaviorA.zipWithPar(behaviorB)(f)

  final def mapN[SIn, SA, SB, SC, Msg, R, E, A, B, C, SOut, Result](
    behaviorA: Step[SIn, SA, Msg, R, E, A],
    behaviorB: Step[SIn, SB, Msg, R, E, B],
    behaviorC: Step[SIn, SC, Msg, R, E, C]
  )(
    f: (BehaviorSuccess[SA, A], BehaviorSuccess[SB, B], BehaviorSuccess[SC, C]) => BehaviorSuccess[SOut, Result]
  ): Step[SIn, SOut, Msg, R, E, Result] =
    (behaviorA <*> behaviorB <*> behaviorC).mapResults { case BehaviorSuccess(((sa, sb), sc), ((a, b), c)) =>
      f(BehaviorSuccess(sa, a), BehaviorSuccess(sb, b), BehaviorSuccess(sc, c))
    }

  final def mapParN[SIn, SA, SB, SC, Msg, R, E, A, B, C, SOut, Result](
    behaviorA: Step[SIn, SA, Msg, R, E, A],
    behaviorB: Step[SIn, SB, Msg, R, E, B],
    behaviorC: Step[SIn, SC, Msg, R, E, C]
  )(
    f: (BehaviorSuccess[SA, A], BehaviorSuccess[SB, B], BehaviorSuccess[SC, C]) => BehaviorSuccess[SOut, Result]
  ): Step[SIn, SOut, Msg, R, E, Result] =
    (behaviorA <&> behaviorB <&> behaviorC).mapResults { case BehaviorSuccess(((sa, sb), sc), ((a, b), c)) =>
      f(BehaviorSuccess(sa, a), BehaviorSuccess(sb, b), BehaviorSuccess(sc, c))
    }

  final def mapN[SIn, SA, SB, SC, SD, Msg, R, E, A, B, C, D, SOut, Result](
    behaviorA: Step[SIn, SA, Msg, R, E, A],
    behaviorB: Step[SIn, SB, Msg, R, E, B],
    behaviorC: Step[SIn, SC, Msg, R, E, C],
    behaviorD: Step[SIn, SD, Msg, R, E, D]
  )(
    f: (
      BehaviorSuccess[SA, A],
      BehaviorSuccess[SB, B],
      BehaviorSuccess[SC, C],
      BehaviorSuccess[SD, D]
    ) => BehaviorSuccess[SOut, Result]
  ): Step[SIn, SOut, Msg, R, E, Result] =
    (behaviorA <*> behaviorB <*> behaviorC <*> behaviorD).mapResults {
      case BehaviorSuccess((((sa, sb), sc), sd), (((a, b), c), d)) =>
        f(BehaviorSuccess(sa, a), BehaviorSuccess(sb, b), BehaviorSuccess(sc, c), BehaviorSuccess(sd, d))
    }

  final def mapParN[SIn, SA, SB, SC, SD, Msg, R, E, A, B, C, D, SOut, Result](
    behaviorA: Step[SIn, SA, Msg, R, E, A],
    behaviorB: Step[SIn, SB, Msg, R, E, B],
    behaviorC: Step[SIn, SC, Msg, R, E, C],
    behaviorD: Step[SIn, SD, Msg, R, E, D]
  )(
    f: (
      BehaviorSuccess[SA, A],
      BehaviorSuccess[SB, B],
      BehaviorSuccess[SC, C],
      BehaviorSuccess[SD, D]
    ) => BehaviorSuccess[SOut, Result]
  ): Step[SIn, SOut, Msg, R, E, Result] =
    (behaviorA <&> behaviorB <&> behaviorC <&> behaviorD).mapResults {
      case BehaviorSuccess((((sa, sb), sc), sd), (((a, b), c), d)) =>
        f(BehaviorSuccess(sa, a), BehaviorSuccess(sb, b), BehaviorSuccess(sc, c), BehaviorSuccess(sd, d))
    }

  final def mapN[SIn, SA, SB, SC, SD, SE, Msg, R, Err, A, B, C, D, E, SOut, Result](
    behaviorA: Step[SIn, SA, Msg, R, Err, A],
    behaviorB: Step[SIn, SB, Msg, R, Err, B],
    behaviorC: Step[SIn, SC, Msg, R, Err, C],
    behaviorD: Step[SIn, SD, Msg, R, Err, D],
    behaviorE: Step[SIn, SE, Msg, R, Err, E]
  )(
    f: (
      BehaviorSuccess[SA, A],
      BehaviorSuccess[SB, B],
      BehaviorSuccess[SC, C],
      BehaviorSuccess[SD, D],
      BehaviorSuccess[SE, E]
    ) => BehaviorSuccess[SOut, Result]
  ): Step[SIn, SOut, Msg, R, Err, Result] =
    (behaviorA <*> behaviorB <*> behaviorC <*> behaviorD <*> behaviorE).mapResults {
      case BehaviorSuccess(((((sa, sb), sc), sd), se), ((((a, b), c), d), e)) =>
        f(
          BehaviorSuccess(sa, a),
          BehaviorSuccess(sb, b),
          BehaviorSuccess(sc, c),
          BehaviorSuccess(sd, d),
          BehaviorSuccess(se, e)
        )
    }

  final def mapParN[SIn, SA, SB, SC, SD, SE, Msg, R, Err, A, B, C, D, E, SOut, Result](
    behaviorA: Step[SIn, SA, Msg, R, Err, A],
    behaviorB: Step[SIn, SB, Msg, R, Err, B],
    behaviorC: Step[SIn, SC, Msg, R, Err, C],
    behaviorD: Step[SIn, SD, Msg, R, Err, D],
    behaviorE: Step[SIn, SE, Msg, R, Err, E]
  )(
    f: (
      BehaviorSuccess[SA, A],
      BehaviorSuccess[SB, B],
      BehaviorSuccess[SC, C],
      BehaviorSuccess[SD, D],
      BehaviorSuccess[SE, E]
    ) => BehaviorSuccess[SOut, Result]
  ): Step[SIn, SOut, Msg, R, Err, Result] =
    (behaviorA <&> behaviorB <&> behaviorC <&> behaviorD <&> behaviorE).mapResults {
      case BehaviorSuccess(((((sa, sb), sc), sd), se), ((((a, b), c), d), e)) =>
        f(
          BehaviorSuccess(sa, a),
          BehaviorSuccess(sb, b),
          BehaviorSuccess(sc, c),
          BehaviorSuccess(sd, d),
          BehaviorSuccess(se, e)
        )
    }

  final def mapN[SIn, SA, SB, SC, SD, SE, SF, Msg, R, Err, A, B, C, D, E, F, SOut, Result](
    behaviorA: Step[SIn, SA, Msg, R, Err, A],
    behaviorB: Step[SIn, SB, Msg, R, Err, B],
    behaviorC: Step[SIn, SC, Msg, R, Err, C],
    behaviorD: Step[SIn, SD, Msg, R, Err, D],
    behaviorE: Step[SIn, SE, Msg, R, Err, E],
    behaviorF: Step[SIn, SF, Msg, R, Err, F]
  )(
    fn: (
      BehaviorSuccess[SA, A],
      BehaviorSuccess[SB, B],
      BehaviorSuccess[SC, C],
      BehaviorSuccess[SD, D],
      BehaviorSuccess[SE, E],
      BehaviorSuccess[SF, F]
    ) => BehaviorSuccess[SOut, Result]
  ): Step[SIn, SOut, Msg, R, Err, Result] =
    (behaviorA <*> behaviorB <*> behaviorC <*> behaviorD <*> behaviorE <*> behaviorF).mapResults {
      case BehaviorSuccess((((((sa, sb), sc), sd), se), sf), (((((a, b), c), d), e), f)) =>
        fn(
          BehaviorSuccess(sa, a),
          BehaviorSuccess(sb, b),
          BehaviorSuccess(sc, c),
          BehaviorSuccess(sd, d),
          BehaviorSuccess(se, e),
          BehaviorSuccess(sf, f)
        )
    }

  final def mapParN[SIn, SA, SB, SC, SD, SE, SF, Msg, R, Err, A, B, C, D, E, F, SOut, Result](
    behaviorA: Step[SIn, SA, Msg, R, Err, A],
    behaviorB: Step[SIn, SB, Msg, R, Err, B],
    behaviorC: Step[SIn, SC, Msg, R, Err, C],
    behaviorD: Step[SIn, SD, Msg, R, Err, D],
    behaviorE: Step[SIn, SE, Msg, R, Err, E],
    behaviorF: Step[SIn, SF, Msg, R, Err, F]
  )(
    fn: (
      BehaviorSuccess[SA, A],
      BehaviorSuccess[SB, B],
      BehaviorSuccess[SC, C],
      BehaviorSuccess[SD, D],
      BehaviorSuccess[SE, E],
      BehaviorSuccess[SF, F]
    ) => BehaviorSuccess[SOut, Result]
  ): Step[SIn, SOut, Msg, R, Err, Result] =
    (behaviorA <&> behaviorB <&> behaviorC <&> behaviorD <&> behaviorE <&> behaviorF).mapResults {
      case BehaviorSuccess((((((sa, sb), sc), sd), se), sf), (((((a, b), c), d), e), f)) =>
        fn(
          BehaviorSuccess(sa, a),
          BehaviorSuccess(sb, b),
          BehaviorSuccess(sc, c),
          BehaviorSuccess(sd, d),
          BehaviorSuccess(se, e),
          BehaviorSuccess(sf, f)
        )
    }

  final def mapN[SIn, SA, SB, SC, SD, SE, SF, SG, Msg, R, Err, A, B, C, D, E, F, G, SOut, Result](
    behaviorA: Step[SIn, SA, Msg, R, Err, A],
    behaviorB: Step[SIn, SB, Msg, R, Err, B],
    behaviorC: Step[SIn, SC, Msg, R, Err, C],
    behaviorD: Step[SIn, SD, Msg, R, Err, D],
    behaviorE: Step[SIn, SE, Msg, R, Err, E],
    behaviorF: Step[SIn, SF, Msg, R, Err, F],
    behaviorG: Step[SIn, SG, Msg, R, Err, G]
  )(
    fn: (
      BehaviorSuccess[SA, A],
      BehaviorSuccess[SB, B],
      BehaviorSuccess[SC, C],
      BehaviorSuccess[SD, D],
      BehaviorSuccess[SE, E],
      BehaviorSuccess[SF, F],
      BehaviorSuccess[SG, G]
    ) => BehaviorSuccess[SOut, Result]
  ): Step[SIn, SOut, Msg, R, Err, Result] =
    (behaviorA <*> behaviorB <*> behaviorC <*> behaviorD <*> behaviorE <*> behaviorF <*> behaviorG).mapResults {
      case BehaviorSuccess(((((((sa, sb), sc), sd), se), sf), sg), ((((((a, b), c), d), e), f), g)) =>
        fn(
          BehaviorSuccess(sa, a),
          BehaviorSuccess(sb, b),
          BehaviorSuccess(sc, c),
          BehaviorSuccess(sd, d),
          BehaviorSuccess(se, e),
          BehaviorSuccess(sf, f),
          BehaviorSuccess(sg, g)
        )
    }

  final def mapParN[SIn, SA, SB, SC, SD, SE, SF, SG, Msg, R, Err, A, B, C, D, E, F, G, SOut, Result](
    behaviorA: Step[SIn, SA, Msg, R, Err, A],
    behaviorB: Step[SIn, SB, Msg, R, Err, B],
    behaviorC: Step[SIn, SC, Msg, R, Err, C],
    behaviorD: Step[SIn, SD, Msg, R, Err, D],
    behaviorE: Step[SIn, SE, Msg, R, Err, E],
    behaviorF: Step[SIn, SF, Msg, R, Err, F],
    behaviorG: Step[SIn, SG, Msg, R, Err, G]
  )(
    fn: (
      BehaviorSuccess[SA, A],
      BehaviorSuccess[SB, B],
      BehaviorSuccess[SC, C],
      BehaviorSuccess[SD, D],
      BehaviorSuccess[SE, E],
      BehaviorSuccess[SF, F],
      BehaviorSuccess[SG, G]
    ) => BehaviorSuccess[SOut, Result]
  ): Step[SIn, SOut, Msg, R, Err, Result] =
    (behaviorA <&> behaviorB <&> behaviorC <&> behaviorD <&> behaviorE <&> behaviorF <&> behaviorG).mapResults {
      case BehaviorSuccess(((((((sa, sb), sc), sd), se), sf), sg), ((((((a, b), c), d), e), f), g)) =>
        fn(
          BehaviorSuccess(sa, a),
          BehaviorSuccess(sb, b),
          BehaviorSuccess(sc, c),
          BehaviorSuccess(sd, d),
          BehaviorSuccess(se, e),
          BehaviorSuccess(sf, f),
          BehaviorSuccess(sg, g)
        )
    }

  final def mapN[SIn, SA, SB, SC, SD, SE, SF, SG, SH, Msg, R, Err, A, B, C, D, E, F, G, H, SOut, Result](
    behaviorA: Step[SIn, SA, Msg, R, Err, A],
    behaviorB: Step[SIn, SB, Msg, R, Err, B],
    behaviorC: Step[SIn, SC, Msg, R, Err, C],
    behaviorD: Step[SIn, SD, Msg, R, Err, D],
    behaviorE: Step[SIn, SE, Msg, R, Err, E],
    behaviorF: Step[SIn, SF, Msg, R, Err, F],
    behaviorG: Step[SIn, SG, Msg, R, Err, G],
    behaviorH: Step[SIn, SH, Msg, R, Err, H]
  )(
    fn: (
      BehaviorSuccess[SA, A],
      BehaviorSuccess[SB, B],
      BehaviorSuccess[SC, C],
      BehaviorSuccess[SD, D],
      BehaviorSuccess[SE, E],
      BehaviorSuccess[SF, F],
      BehaviorSuccess[SG, G],
      BehaviorSuccess[SH, H]
    ) => BehaviorSuccess[SOut, Result]
  ): Step[SIn, SOut, Msg, R, Err, Result] =
    (behaviorA <*> behaviorB <*> behaviorC <*> behaviorD <*> behaviorE <*> behaviorF <*> behaviorG <*> behaviorH).mapResults {
      case BehaviorSuccess((((((((sa, sb), sc), sd), se), sf), sg), sh), (((((((a, b), c), d), e), f), g), h)) =>
        fn(
          BehaviorSuccess(sa, a),
          BehaviorSuccess(sb, b),
          BehaviorSuccess(sc, c),
          BehaviorSuccess(sd, d),
          BehaviorSuccess(se, e),
          BehaviorSuccess(sf, f),
          BehaviorSuccess(sg, g),
          BehaviorSuccess(sh, h)
        )
    }

  final def mapParN[SIn, SA, SB, SC, SD, SE, SF, SG, SH, Msg, R, Err, A, B, C, D, E, F, G, H, SOut, Result](
    behaviorA: Step[SIn, SA, Msg, R, Err, A],
    behaviorB: Step[SIn, SB, Msg, R, Err, B],
    behaviorC: Step[SIn, SC, Msg, R, Err, C],
    behaviorD: Step[SIn, SD, Msg, R, Err, D],
    behaviorE: Step[SIn, SE, Msg, R, Err, E],
    behaviorF: Step[SIn, SF, Msg, R, Err, F],
    behaviorG: Step[SIn, SG, Msg, R, Err, G],
    behaviorH: Step[SIn, SH, Msg, R, Err, H]
  )(
    fn: (
      BehaviorSuccess[SA, A],
      BehaviorSuccess[SB, B],
      BehaviorSuccess[SC, C],
      BehaviorSuccess[SD, D],
      BehaviorSuccess[SE, E],
      BehaviorSuccess[SF, F],
      BehaviorSuccess[SG, G],
      BehaviorSuccess[SH, H]
    ) => BehaviorSuccess[SOut, Result]
  ): Step[SIn, SOut, Msg, R, Err, Result] =
    (behaviorA <&> behaviorB <&> behaviorC <&> behaviorD <&> behaviorE <&> behaviorF <&> behaviorG <&> behaviorH).mapResults {
      case BehaviorSuccess((((((((sa, sb), sc), sd), se), sf), sg), sh), (((((((a, b), c), d), e), f), g), h)) =>
        fn(
          BehaviorSuccess(sa, a),
          BehaviorSuccess(sb, b),
          BehaviorSuccess(sc, c),
          BehaviorSuccess(sd, d),
          BehaviorSuccess(se, e),
          BehaviorSuccess(sf, f),
          BehaviorSuccess(sg, g),
          BehaviorSuccess(sh, h)
        )
    }

  final implicit def toSuccessMergeFunc[SA, A, SB, B, SOut, Result](
    fn: ((SA, A), (SB, B)) => (SOut, Result)
  ): (BehaviorSuccess[SA, A], BehaviorSuccess[SB, B]) => BehaviorSuccess[SOut, Result] = { case (successA, successB) =>
    fn(successA.toTuple, successB.toTuple)
  }

  final implicit def toSuccessMergeFunc[SA, A, SB, B, SC, C, SOut, Result](
    fn: ((SA, A), (SB, B), (SC, C)) => (SOut, Result)
  ): (BehaviorSuccess[SA, A], BehaviorSuccess[SB, B], BehaviorSuccess[SC, C]) => BehaviorSuccess[SOut, Result] = {
    case (successA, successB, successC) =>
      fn(successA.toTuple, successB.toTuple, successC.toTuple)
  }

  final implicit def toSuccessMergeFunc[SA, A, SB, B, SC, C, SD, D, SOut, Result](
    fn: ((SA, A), (SB, B), (SC, C), (SD, D)) => (SOut, Result)
  ): (
    BehaviorSuccess[SA, A],
    BehaviorSuccess[SB, B],
    BehaviorSuccess[SC, C],
    BehaviorSuccess[SD, D]
  ) => BehaviorSuccess[SOut, Result] = { case (successA, successB, successC, successD) =>
    fn(successA.toTuple, successB.toTuple, successC.toTuple, successD.toTuple)
  }

  final implicit def toSuccessMergeFunc[SA, A, SB, B, SC, C, SD, D, SE, E, SOut, Result](
    fn: ((SA, A), (SB, B), (SC, C), (SD, D), (SE, E)) => (SOut, Result)
  ): (
    BehaviorSuccess[SA, A],
    BehaviorSuccess[SB, B],
    BehaviorSuccess[SC, C],
    BehaviorSuccess[SD, D],
    BehaviorSuccess[SE, E]
  ) => BehaviorSuccess[SOut, Result] = { case (successA, successB, successC, successD, successE) =>
    fn(successA.toTuple, successB.toTuple, successC.toTuple, successD.toTuple, successE.toTuple)
  }

  final implicit def toSuccessMergeFunc[SA, A, SB, B, SC, C, SD, D, SE, E, SF, F, SOut, Result](
    fn: ((SA, A), (SB, B), (SC, C), (SD, D), (SE, E), (SF, F)) => (SOut, Result)
  ): (
    BehaviorSuccess[SA, A],
    BehaviorSuccess[SB, B],
    BehaviorSuccess[SC, C],
    BehaviorSuccess[SD, D],
    BehaviorSuccess[SE, E],
    BehaviorSuccess[SF, F]
  ) => BehaviorSuccess[SOut, Result] = { case (successA, successB, successC, successD, successE, successF) =>
    fn(successA.toTuple, successB.toTuple, successC.toTuple, successD.toTuple, successE.toTuple, successF.toTuple)
  }

  final implicit def toSuccessMergeFunc[SA, A, SB, B, SC, C, SD, D, SE, E, SF, F, SG, G, SOut, Result](
    fn: ((SA, A), (SB, B), (SC, C), (SD, D), (SE, E), (SF, F), (SG, G)) => (SOut, Result)
  ): (
    BehaviorSuccess[SA, A],
    BehaviorSuccess[SB, B],
    BehaviorSuccess[SC, C],
    BehaviorSuccess[SD, D],
    BehaviorSuccess[SE, E],
    BehaviorSuccess[SF, F],
    BehaviorSuccess[SG, G]
  ) => BehaviorSuccess[SOut, Result] = { case (successA, successB, successC, successD, successE, successF, successG) =>
    fn(
      successA.toTuple,
      successB.toTuple,
      successC.toTuple,
      successD.toTuple,
      successE.toTuple,
      successF.toTuple,
      successG.toTuple
    )
  }

  final implicit def toSuccessMergeFunc[SA, A, SB, B, SC, C, SD, D, SE, E, SF, F, SG, G, SH, H, SOut, Result](
    fn: ((SA, A), (SB, B), (SC, C), (SD, D), (SE, E), (SF, F), (SG, G), (SH, H)) => (SOut, Result)
  ): (
    BehaviorSuccess[SA, A],
    BehaviorSuccess[SB, B],
    BehaviorSuccess[SC, C],
    BehaviorSuccess[SD, D],
    BehaviorSuccess[SE, E],
    BehaviorSuccess[SF, F],
    BehaviorSuccess[SG, G],
    BehaviorSuccess[SH, H]
  ) => BehaviorSuccess[SOut, Result] = {
    case (successA, successB, successC, successD, successE, successF, successG, successH) =>
      fn(
        successA.toTuple,
        successB.toTuple,
        successC.toTuple,
        successD.toTuple,
        successE.toTuple,
        successF.toTuple,
        successG.toTuple,
        successH.toTuple
      )
  }

}

object mapNExamples extends zio.App {
  import zio._
  def run(args: List[String]): URIO[ZEnv, ExitCode] = {
    val behaviorA = Step.set("SA").as('A')
    val behaviorB = Step.set(List("SA")).as("B")
    val finalBehavior = Step.mapN(behaviorA, behaviorB) { case (a, b) =>
      BehaviorSuccess(state = (a.state, b.state), result = (a.result, b.result))
    }

    val finalBehaviorAlt = Step.mapN(behaviorA, behaviorB) { case (a, b) =>
      ((a.state, b.state), (a.result, b.result))
    }

    (finalBehavior.run.tap(res => console.putStrLn(s"Result Orig: $res")) *>
      finalBehaviorAlt.run.tap(res => console.putStrLn(s"Result  Alt: $res"))).exitCode
  }
}
