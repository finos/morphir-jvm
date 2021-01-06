package morphir.flowz.spark.sample.filtering

import morphir.flowz.spark.api._

import zio._
object DataFrameFilteringSample extends App {
  def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    val getProducts = SparkStep.createDataset(
      Seq(
        Product("Surface Book", "Microsoft"),
        Product("iPhone", "Apple"),
        Product("Galaxy Tab", "Samsung"),
        Product("Galaxy", "Samsung"),
        Product("Pixel", "Google"),
        Product("Macbook Pro", "Apple")
      )
    )

    getProducts.run(args).exitCode
    ???
  }

  final case class Product(name: String, manufacturer: String)
}
