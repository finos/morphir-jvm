package zio.morphir.ir
import zio.test.*

object Gens {
  val greeWords = Gen.oneOf(
    Seq(
      "alpha",
      "beta",
      "gamma",
      "delta",
      "epsilon",
      "zeta",
      "eta",
      "theta",
      "iota",
      "kappa",
      "lambda",
      "mu",
      "nu",
      "xi",
      "omicron",
      "pi",
      "rho",
      "sigma",
      "tau",
      "upsilon",
      "phi",
      "chi",
      "psi",
      "omega"
    ).map(Gen.const(_)): _*
  )
}
