package morphir

/**
 * Generated based on ListOfResults
 */
object ListOfResults {

  def liftAllErrors[A, E](
    results: morphir.sdk.List.List[morphir.sdk.Result.Result[E, A]]
  ): morphir.sdk.Result.Result[morphir.sdk.List.List[E], morphir.sdk.List.List[A]] = {
    val oks: morphir.sdk.List.List[A] = morphir.sdk.List.filterMap(
      ((result: morphir.sdk.Result.Result[E, A]) => morphir.sdk.Result.toMaybe(result))
    )(results)

    val errs: morphir.sdk.List.List[E] = morphir.sdk.List.filterMap(
      (
        (result: morphir.sdk.Result.Result[E, A]) =>
          result match {
            case morphir.sdk.Result.Ok(_) =>
              (morphir.sdk.Maybe.Nothing: morphir.sdk.Maybe.Maybe[E])
            case morphir.sdk.Result.Err(e) =>
              (morphir.sdk.Maybe.Just(e): morphir.sdk.Maybe.Maybe[E])
          }
      )
    )(results)

    errs match {
      case Nil =>
        (morphir.sdk.Result.Ok(oks): morphir.sdk.Result.Result[morphir.sdk.List.List[E], morphir.sdk.List.List[A]])
      case _ =>
        (morphir.sdk.Result.Err(errs): morphir.sdk.Result.Result[morphir.sdk.List.List[E], morphir.sdk.List.List[A]])
    }
  }

  def liftFirstError[A, E](
    results: morphir.sdk.List.List[morphir.sdk.Result.Result[E, A]]
  ): morphir.sdk.Result.Result[E, morphir.sdk.List.List[A]] =
    morphir.ListOfResults.liftAllErrors(results) match {
      case morphir.sdk.Result.Ok(a) =>
        (morphir.sdk.Result.Ok(a): morphir.sdk.Result.Result[E, morphir.sdk.List.List[A]])
      case morphir.sdk.Result.Err(errors) =>
        morphir.sdk.Maybe.withDefault(
          (morphir.sdk.Result.Ok(
            morphir.sdk.List(
            )
          ): morphir.sdk.Result.Result[E, morphir.sdk.List.List[A]])
        )(morphir.sdk.Maybe.map((morphir.sdk.Result.Err(_: E)))(morphir.sdk.List.head(errors)))
    }

}
