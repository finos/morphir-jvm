module Arithmetic.ColorWithThreeValues exposing (ColorWithThreeValues(..))

type ColorWithThreeValues
    = Named String Int Int
    | Custom Int Int Int
