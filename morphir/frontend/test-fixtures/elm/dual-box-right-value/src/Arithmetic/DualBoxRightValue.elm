module Arithmetic.DualBoxRightValue exposing (dualBoxRightValue)

import Arithmetic.DualBoxContainer exposing (DualBoxContainer)

dualBoxRightValue : DualBoxContainer -> String
dualBoxRightValue container =
    container.box.right
