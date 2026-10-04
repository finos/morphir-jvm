module A.B.UseNestedAddOne exposing (addTwo)

import A.B.C.AddOne exposing (addOne)

addTwo : Int -> Int
addTwo value =
    addOne value + 1
