package morphir.engine.processor

import morphir.engine.plugin.Plugin

sealed abstract class Processor {
  def plugins: List[Plugin]
}
