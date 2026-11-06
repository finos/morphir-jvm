module Arithmetic.DualBoxContainer exposing (DualBoxContainer)

import Arithmetic.DualBox exposing (DualBox)

type alias DualBoxContainer =
    { box : DualBox Int String
    , fee : Int
    }
