package org.finos.morphir.ir
import scala.collection.immutable.ListMap

final case class PackageSpecification[+A](modules: ListMap[ModuleName, ModuleSpecification[A]])

object PackageSpecification:
  def empty[A]: PackageSpecification[A] =
    PackageSpecification[A](ListMap.empty)
