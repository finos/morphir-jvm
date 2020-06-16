package morphir.ir

import zio.Has

package object rewriter {
  type Rewriter = Has[Rewriter.Service]
}
