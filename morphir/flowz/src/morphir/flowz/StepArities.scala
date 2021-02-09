package morphir.flowz

import zio.logging.LogLevel

trait StepArities {

  final def mapN[SIn, SA, SB, Msg, R, E, A, B, SOut, Result, MA, MB](
    behaviorA: Step[SIn, SA, Msg, R, E, A],
    behaviorB: Step[SIn, SB, Msg, R, E, B]
  )(
    f: (StepSuccess[SA, A], StepSuccess[SB, B]) => StepSuccess[SOut, Result]
  ): Step[SIn, SOut, Msg, R, E, Result] =
    behaviorA.zipWith(behaviorB)(f)

  final def mapParN[SIn, SA, SB, Msg, R, E, A, B, SOut, Result](
    behaviorA: Step[SIn, SA, Msg, R, E, A],
    behaviorB: Step[SIn, SB, Msg, R, E, B]
  )(
    f: (StepSuccess[SA, A], StepSuccess[SB, B]) => StepSuccess[SOut, Result]
  ): Step[SIn, SOut, Msg, R, E, Result] =
    behaviorA.zipWithPar(behaviorB)(f)

  final def mapN[SIn, SA, SB, SC, Msg, R, E, A, B, C, SOut, Result](
    behaviorA: Step[SIn, SA, Msg, R, E, A],
    behaviorB: Step[SIn, SB, Msg, R, E, B],
    behaviorC: Step[SIn, SC, Msg, R, E, C]
  )(
    f: (StepSuccess[SA, A], StepSuccess[SB, B], StepSuccess[SC, C]) => StepSuccess[SOut, Result]
  ): Step[SIn, SOut, Msg, R, E, Result] =
    (behaviorA <*> behaviorB <*> behaviorC).mapResults { case StepSuccess(((sa, sb), sc), ((a, b), c)) =>
      f(StepSuccess(sa, a), StepSuccess(sb, b), StepSuccess(sc, c))
    }

  final def mapParN[SIn, SA, SB, SC, Msg, R, E, A, B, C, SOut, Result](
    behaviorA: Step[SIn, SA, Msg, R, E, A],
    behaviorB: Step[SIn, SB, Msg, R, E, B],
    behaviorC: Step[SIn, SC, Msg, R, E, C]
  )(
    f: (StepSuccess[SA, A], StepSuccess[SB, B], StepSuccess[SC, C]) => StepSuccess[SOut, Result]
  ): Step[SIn, SOut, Msg, R, E, Result] =
    (behaviorA <&> behaviorB <&> behaviorC).mapResults { case StepSuccess(((sa, sb), sc), ((a, b), c)) =>
      f(StepSuccess(sa, a), StepSuccess(sb, b), StepSuccess(sc, c))
    }

  final def mapN[SIn, SA, SB, SC, SD, Msg, R, E, A, B, C, D, SOut, Result](
    behaviorA: Step[SIn, SA, Msg, R, E, A],
    behaviorB: Step[SIn, SB, Msg, R, E, B],
    behaviorC: Step[SIn, SC, Msg, R, E, C],
    behaviorD: Step[SIn, SD, Msg, R, E, D]
  )(
    f: (
      StepSuccess[SA, A],
      StepSuccess[SB, B],
      StepSuccess[SC, C],
      StepSuccess[SD, D]
    ) => StepSuccess[SOut, Result]
  ): Step[SIn, SOut, Msg, R, E, Result] =
    (behaviorA <*> behaviorB <*> behaviorC <*> behaviorD).mapResults {
      case StepSuccess((((sa, sb), sc), sd), (((a, b), c), d)) =>
        f(StepSuccess(sa, a), StepSuccess(sb, b), StepSuccess(sc, c), StepSuccess(sd, d))
    }

  final def mapParN[SIn, SA, SB, SC, SD, Msg, R, E, A, B, C, D, SOut, Result](
    behaviorA: Step[SIn, SA, Msg, R, E, A],
    behaviorB: Step[SIn, SB, Msg, R, E, B],
    behaviorC: Step[SIn, SC, Msg, R, E, C],
    behaviorD: Step[SIn, SD, Msg, R, E, D]
  )(
    f: (
      StepSuccess[SA, A],
      StepSuccess[SB, B],
      StepSuccess[SC, C],
      StepSuccess[SD, D]
    ) => StepSuccess[SOut, Result]
  ): Step[SIn, SOut, Msg, R, E, Result] =
    (behaviorA <&> behaviorB <&> behaviorC <&> behaviorD).mapResults {
      case StepSuccess((((sa, sb), sc), sd), (((a, b), c), d)) =>
        f(StepSuccess(sa, a), StepSuccess(sb, b), StepSuccess(sc, c), StepSuccess(sd, d))
    }

  final def mapN[SIn, SA, SB, SC, SD, SE, Msg, R, Err, A, B, C, D, E, SOut, Result](
    behaviorA: Step[SIn, SA, Msg, R, Err, A],
    behaviorB: Step[SIn, SB, Msg, R, Err, B],
    behaviorC: Step[SIn, SC, Msg, R, Err, C],
    behaviorD: Step[SIn, SD, Msg, R, Err, D],
    behaviorE: Step[SIn, SE, Msg, R, Err, E]
  )(
    f: (
      StepSuccess[SA, A],
      StepSuccess[SB, B],
      StepSuccess[SC, C],
      StepSuccess[SD, D],
      StepSuccess[SE, E]
    ) => StepSuccess[SOut, Result]
  ): Step[SIn, SOut, Msg, R, Err, Result] =
    (behaviorA <*> behaviorB <*> behaviorC <*> behaviorD <*> behaviorE).mapResults {
      case StepSuccess(((((sa, sb), sc), sd), se), ((((a, b), c), d), e)) =>
        f(
          StepSuccess(sa, a),
          StepSuccess(sb, b),
          StepSuccess(sc, c),
          StepSuccess(sd, d),
          StepSuccess(se, e)
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
      StepSuccess[SA, A],
      StepSuccess[SB, B],
      StepSuccess[SC, C],
      StepSuccess[SD, D],
      StepSuccess[SE, E]
    ) => StepSuccess[SOut, Result]
  ): Step[SIn, SOut, Msg, R, Err, Result] =
    (behaviorA <&> behaviorB <&> behaviorC <&> behaviorD <&> behaviorE).mapResults {
      case StepSuccess(((((sa, sb), sc), sd), se), ((((a, b), c), d), e)) =>
        f(
          StepSuccess(sa, a),
          StepSuccess(sb, b),
          StepSuccess(sc, c),
          StepSuccess(sd, d),
          StepSuccess(se, e)
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
      StepSuccess[SA, A],
      StepSuccess[SB, B],
      StepSuccess[SC, C],
      StepSuccess[SD, D],
      StepSuccess[SE, E],
      StepSuccess[SF, F]
    ) => StepSuccess[SOut, Result]
  ): Step[SIn, SOut, Msg, R, Err, Result] =
    (behaviorA <*> behaviorB <*> behaviorC <*> behaviorD <*> behaviorE <*> behaviorF).mapResults {
      case StepSuccess((((((sa, sb), sc), sd), se), sf), (((((a, b), c), d), e), f)) =>
        fn(
          StepSuccess(sa, a),
          StepSuccess(sb, b),
          StepSuccess(sc, c),
          StepSuccess(sd, d),
          StepSuccess(se, e),
          StepSuccess(sf, f)
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
      StepSuccess[SA, A],
      StepSuccess[SB, B],
      StepSuccess[SC, C],
      StepSuccess[SD, D],
      StepSuccess[SE, E],
      StepSuccess[SF, F]
    ) => StepSuccess[SOut, Result]
  ): Step[SIn, SOut, Msg, R, Err, Result] =
    (behaviorA <&> behaviorB <&> behaviorC <&> behaviorD <&> behaviorE <&> behaviorF).mapResults {
      case StepSuccess((((((sa, sb), sc), sd), se), sf), (((((a, b), c), d), e), f)) =>
        fn(
          StepSuccess(sa, a),
          StepSuccess(sb, b),
          StepSuccess(sc, c),
          StepSuccess(sd, d),
          StepSuccess(se, e),
          StepSuccess(sf, f)
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
      StepSuccess[SA, A],
      StepSuccess[SB, B],
      StepSuccess[SC, C],
      StepSuccess[SD, D],
      StepSuccess[SE, E],
      StepSuccess[SF, F],
      StepSuccess[SG, G]
    ) => StepSuccess[SOut, Result]
  ): Step[SIn, SOut, Msg, R, Err, Result] =
    (behaviorA <*> behaviorB <*> behaviorC <*> behaviorD <*> behaviorE <*> behaviorF <*> behaviorG).mapResults {
      case StepSuccess(((((((sa, sb), sc), sd), se), sf), sg), ((((((a, b), c), d), e), f), g)) =>
        fn(
          StepSuccess(sa, a),
          StepSuccess(sb, b),
          StepSuccess(sc, c),
          StepSuccess(sd, d),
          StepSuccess(se, e),
          StepSuccess(sf, f),
          StepSuccess(sg, g)
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
      StepSuccess[SA, A],
      StepSuccess[SB, B],
      StepSuccess[SC, C],
      StepSuccess[SD, D],
      StepSuccess[SE, E],
      StepSuccess[SF, F],
      StepSuccess[SG, G]
    ) => StepSuccess[SOut, Result]
  ): Step[SIn, SOut, Msg, R, Err, Result] =
    (behaviorA <&> behaviorB <&> behaviorC <&> behaviorD <&> behaviorE <&> behaviorF <&> behaviorG).mapResults {
      case StepSuccess(((((((sa, sb), sc), sd), se), sf), sg), ((((((a, b), c), d), e), f), g)) =>
        fn(
          StepSuccess(sa, a),
          StepSuccess(sb, b),
          StepSuccess(sc, c),
          StepSuccess(sd, d),
          StepSuccess(se, e),
          StepSuccess(sf, f),
          StepSuccess(sg, g)
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
      StepSuccess[SA, A],
      StepSuccess[SB, B],
      StepSuccess[SC, C],
      StepSuccess[SD, D],
      StepSuccess[SE, E],
      StepSuccess[SF, F],
      StepSuccess[SG, G],
      StepSuccess[SH, H]
    ) => StepSuccess[SOut, Result]
  ): Step[SIn, SOut, Msg, R, Err, Result] =
    (behaviorA <*> behaviorB <*> behaviorC <*> behaviorD <*> behaviorE <*> behaviorF <*> behaviorG <*> behaviorH).mapResults {
      case StepSuccess((((((((sa, sb), sc), sd), se), sf), sg), sh), (((((((a, b), c), d), e), f), g), h)) =>
        fn(
          StepSuccess(sa, a),
          StepSuccess(sb, b),
          StepSuccess(sc, c),
          StepSuccess(sd, d),
          StepSuccess(se, e),
          StepSuccess(sf, f),
          StepSuccess(sg, g),
          StepSuccess(sh, h)
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
      StepSuccess[SA, A],
      StepSuccess[SB, B],
      StepSuccess[SC, C],
      StepSuccess[SD, D],
      StepSuccess[SE, E],
      StepSuccess[SF, F],
      StepSuccess[SG, G],
      StepSuccess[SH, H]
    ) => StepSuccess[SOut, Result]
  ): Step[SIn, SOut, Msg, R, Err, Result] =
    (behaviorA <&> behaviorB <&> behaviorC <&> behaviorD <&> behaviorE <&> behaviorF <&> behaviorG <&> behaviorH).mapResults {
      case StepSuccess((((((((sa, sb), sc), sd), se), sf), sg), sh), (((((((a, b), c), d), e), f), g), h)) =>
        fn(
          StepSuccess(sa, a),
          StepSuccess(sb, b),
          StepSuccess(sc, c),
          StepSuccess(sd, d),
          StepSuccess(se, e),
          StepSuccess(sf, f),
          StepSuccess(sg, g),
          StepSuccess(sh, h)
        )
    }

  final implicit def toSuccessMergeFunc[SA, A, SB, B, SOut, Result](
    fn: ((SA, A), (SB, B)) => (SOut, Result)
  ): (StepSuccess[SA, A], StepSuccess[SB, B]) => StepSuccess[SOut, Result] = { case (successA, successB) =>
    fn(successA.toTuple, successB.toTuple)
  }

  final implicit def toSuccessMergeFunc[SA, A, SB, B, SC, C, SOut, Result](
    fn: ((SA, A), (SB, B), (SC, C)) => (SOut, Result)
  ): (StepSuccess[SA, A], StepSuccess[SB, B], StepSuccess[SC, C]) => StepSuccess[SOut, Result] = {
    case (successA, successB, successC) =>
      fn(successA.toTuple, successB.toTuple, successC.toTuple)
  }

  final implicit def toSuccessMergeFunc[SA, A, SB, B, SC, C, SD, D, SOut, Result](
    fn: ((SA, A), (SB, B), (SC, C), (SD, D)) => (SOut, Result)
  ): (
    StepSuccess[SA, A],
    StepSuccess[SB, B],
    StepSuccess[SC, C],
    StepSuccess[SD, D]
  ) => StepSuccess[SOut, Result] = { case (successA, successB, successC, successD) =>
    fn(successA.toTuple, successB.toTuple, successC.toTuple, successD.toTuple)
  }

  final implicit def toSuccessMergeFunc[SA, A, SB, B, SC, C, SD, D, SE, E, SOut, Result](
    fn: ((SA, A), (SB, B), (SC, C), (SD, D), (SE, E)) => (SOut, Result)
  ): (
    StepSuccess[SA, A],
    StepSuccess[SB, B],
    StepSuccess[SC, C],
    StepSuccess[SD, D],
    StepSuccess[SE, E]
  ) => StepSuccess[SOut, Result] = { case (successA, successB, successC, successD, successE) =>
    fn(successA.toTuple, successB.toTuple, successC.toTuple, successD.toTuple, successE.toTuple)
  }

  final implicit def toSuccessMergeFunc[SA, A, SB, B, SC, C, SD, D, SE, E, SF, F, SOut, Result](
    fn: ((SA, A), (SB, B), (SC, C), (SD, D), (SE, E), (SF, F)) => (SOut, Result)
  ): (
    StepSuccess[SA, A],
    StepSuccess[SB, B],
    StepSuccess[SC, C],
    StepSuccess[SD, D],
    StepSuccess[SE, E],
    StepSuccess[SF, F]
  ) => StepSuccess[SOut, Result] = { case (successA, successB, successC, successD, successE, successF) =>
    fn(successA.toTuple, successB.toTuple, successC.toTuple, successD.toTuple, successE.toTuple, successF.toTuple)
  }

  final implicit def toSuccessMergeFunc[SA, A, SB, B, SC, C, SD, D, SE, E, SF, F, SG, G, SOut, Result](
    fn: ((SA, A), (SB, B), (SC, C), (SD, D), (SE, E), (SF, F), (SG, G)) => (SOut, Result)
  ): (
    StepSuccess[SA, A],
    StepSuccess[SB, B],
    StepSuccess[SC, C],
    StepSuccess[SD, D],
    StepSuccess[SE, E],
    StepSuccess[SF, F],
    StepSuccess[SG, G]
  ) => StepSuccess[SOut, Result] = { case (successA, successB, successC, successD, successE, successF, successG) =>
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
    StepSuccess[SA, A],
    StepSuccess[SB, B],
    StepSuccess[SC, C],
    StepSuccess[SD, D],
    StepSuccess[SE, E],
    StepSuccess[SF, F],
    StepSuccess[SG, G],
    StepSuccess[SH, H]
  ) => StepSuccess[SOut, Result] = {
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
  import morphir.flowz.instrumentation.InstrumentationLogging

  def run(args: List[String]): URIO[ZEnv, ExitCode] = {
    val stepA = step("step A")(Step.set("SA").as('A'))
    val stepB = step("step B")(Step.set(List("SA")).as("B"))
    val finalStep = step("final-1")(Step.mapN(stepA, stepB) { case (a, b) =>
      StepSuccess(state = (a.state, b.state), result = (a.result, b.result))
    })

    val finalStepAlt = step("final-2")(Step.mapN(stepA, stepB) { case (a, b) =>
      ((a.state, b.state), (a.result, b.result))
    })

    (
      (finalStep.run.tap(res => console.putStrLn(s"Result Orig: $res")) *>
        finalStepAlt.run.tap(res => console.putStrLn(s"Result  Alt: $res"))).exitCode
    ).provideCustomLayer(StepUidGenerator.live ++ InstrumentationLogging.console(logLevel = LogLevel.Trace))
  }
}
