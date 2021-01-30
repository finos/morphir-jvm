package morphir.flowz

/**
 * Represents a behavior that does not use state.
 */
abstract class StatelessBehavior[-Msg, -R, +E, +A] extends Behavior[Any, Any, Msg, R, E, A] {}
