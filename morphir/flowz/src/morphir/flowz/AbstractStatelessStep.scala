package morphir.flowz

/**
 * Represents a behavior that does not use state.
 */
abstract class AbstractStatelessStep[-InputMsg, -R, +E, +A] extends Step[Any, Any, InputMsg, R, E, A] {}
