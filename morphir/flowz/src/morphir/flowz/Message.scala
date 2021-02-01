package morphir.flowz

import java.time.Instant

final case class Message[+Payload](payload: Payload, timestamp: Instant) {}
