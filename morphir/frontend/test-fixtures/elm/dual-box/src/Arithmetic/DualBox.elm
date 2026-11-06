module Arithmetic.DualBox exposing (DualBox)

type alias DualBox a b =
    { right : b
    , left : a
    }
