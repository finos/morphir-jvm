package org.finos.morphir.knowledge.core

import scala.reflect.ClassTag

final case class State(
    private val fields: Fields,
    private val fieldConstraints: Map[Field[_], List[FieldConstraint]] = Map.empty
) { self =>

  private def addWithEnforcedConstraints[A: ClassTag](field: Field[A], value: Value): Option[State] = {
    def applyConstraint(state: Option[State], fieldConstraint: FieldConstraint): Option[State] =
      state.collect(fieldConstraint)

    def fieldsConstrainedBy(fieldConstraint: FieldConstraint): List[Field[_]] =
      fieldConstraints.collect { case (field, constraints) if constraints.contains(fieldConstraint) => field }.toList

    val newState    = copy(fields = fields + (field, value))
    val constraints = fieldConstraints.getOrElse(field, List.empty)
    constraints
      .filter { constraint =>
        fieldsConstrainedBy(constraint).forall { constrainedField =>
          newState.hasValue(constrainedField)
        }
      }
      .foldLeft(Option(newState)) { case (state, constraint) =>
        applyConstraint(state, constraint)
      }
  }

  private[knowledge] def addConstraint[A](field: Field[A], constraint: FieldConstraint): State =
    fieldConstraints.get(field) match {
      case Some(constraints) =>
        copy(fieldConstraints = (fieldConstraints - field) + (field -> (constraints :+ constraint)))
      case None => copy(fieldConstraints = fieldConstraints + (field -> List(constraint)))
    }

  private[knowledge] def constraintsOn[A](field: Field[A]): List[FieldConstraint] =
    fieldConstraints.get(field).getOrElse(Nil)

  def dynamicValueOf(value: Value): Value = fields.dynamicValueOf(value)

  private def getConstrainedBy(fieldConstraint: FieldConstraint): List[Field[_]] =
    fieldConstraints.collect { case (field, constraints) if constraints.contains(fieldConstraint) => field }.toList

  private[knowledge] def hasConstraint[A](field: Field[A]): Boolean = fieldConstraints.contains(field)

  def hasValue[A](field: Field[A]): Boolean = fields.hasValue(field)(field.fieldType)

  private[knowledge] def unify(first: Value, second: Value): Option[State] = {

    val firstValue  = dynamicValueOf(first)
    val secondValue = dynamicValueOf(second)
    if (firstValue == secondValue) { Some(self) }
    else {
      (firstValue, secondValue) match {
        case (field @ Field(_, value), _) => addWithEnforcedConstraints(field, value)(field.fieldType)
        case (_, field @ Field(_, value)) => addWithEnforcedConstraints(field, value)(field.fieldType)
        case _                            => None
      }
    }
  }

  def valueOf[A: ClassTag](field: Field[A]): Option[A] = fields.valueOf(field)

  def valuesOf(selected: List[Field[_]]): Fields = selected match {
    case Nil =>
      val selectedFields = fields.associateWithFields(selected)(dynamicValueOf(_))
      Fields(selectedFields)
    case _ =>
      val selectedFields = fields.associateWithFields(selected)(dynamicValueOf(_))
      Fields(selectedFields)
  }

  def valuesOf(selected: Field[_]*): Fields = valuesOf(selected.toList)

  private[knowledge] def withFieldConstraints(fieldConstraints: Map[Field[_], List[FieldConstraint]]): State =
    copy(fieldConstraints = fieldConstraints)

  private[knowledge] def withFieldConstraints(fieldConstraints: (Field[_], List[FieldConstraint])*): State =
    copy(fieldConstraints = fieldConstraints.toMap)

}

object State {
  val empty: State = State(Fields.empty, Map.empty)

  private[knowledge] def fromFieldConstraints(fieldConstraints: Map[Field[_], List[FieldConstraint]]): State =
    State(Fields.empty, fieldConstraints)

  private[knowledge] def fromFieldConstraints(fieldConstraints: (Field[_], List[FieldConstraint])*): State =
    State(Fields.empty, fieldConstraints.toMap)
}
