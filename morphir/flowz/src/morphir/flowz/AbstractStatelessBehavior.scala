package morphir.flowz

/**
 * Represents a behavior that does not use state.
 */
abstract class AbstractStatelessBehavior[-InputMsg, -R, +E, +A] extends Behavior[Any, Any, InputMsg, R, E, A] {}
