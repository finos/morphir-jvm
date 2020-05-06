package morphir.toolbox.binding

import morphir.toolbox.binding.elm.ElmFrontendBinding.ElmFrontendState

package object elm {
  type ElmFrontendBinding = FrontendBinding[ElmFrontendState]
}
