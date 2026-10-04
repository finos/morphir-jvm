module Arithmetic.NestedBoxValue exposing (nestedBoxValue)

import Arithmetic.NestedBoxContainer exposing (NestedBoxContainer)

nestedBoxValue : NestedBoxContainer -> Int
nestedBoxValue container =
    container.box.value
