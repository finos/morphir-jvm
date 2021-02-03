package morphir.flowz

/**
 * Represents a behavior that does not use state.
 */
abstract class AbstractStatelessBehavior[-Msg, -R, +E, +A] extends Behavior[Any, Any, Msg, R, E, A] {}
