module Arithmetic.NestedBoxContainer exposing (NestedBoxContainer)

import Arithmetic.GenericBox exposing (GenericBox)

type alias NestedBoxContainer =
    { box : GenericBox Int
    , label : String
    }
