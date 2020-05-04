package org.morphir.toolbox.binding

import org.morphir.toolbox.binding.elm.ElmFrontendBinding.ElmFrontendState

package object elm {
  type ElmFrontendBinding = FrontendBinding[ElmFrontendState]
}
