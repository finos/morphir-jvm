package morphir.toolbox.core

final case class ProjectName(
  original: String,
  overridden: Option[String] = None
) {
  def resolvedName: String = overridden getOrElse original

  override def toString: String = resolvedName

  override def hashCode(): Int = resolvedName.toLowerCase.hashCode

  override def equals(obj: Any): Boolean = obj match {
    case projectName: ProjectName =>
      projectName.resolvedName.equalsIgnoreCase(resolvedName)
    case _ => false
  }

  def matches(name: String): Boolean =
    name.equalsIgnoreCase(resolvedName) || name.equalsIgnoreCase(original)
}
