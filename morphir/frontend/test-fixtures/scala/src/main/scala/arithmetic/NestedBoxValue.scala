package arithmetic

object NestedBoxValue:
  def nestedBoxValue(container: NestedBoxContainer): Int =
    container.box.value
