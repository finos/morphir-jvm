module Arithmetic.PersonWithAdjustedAge exposing (PersonWithAdjustedAge, adjustedAge)

type alias PersonWithAdjustedAge =
    { age : Int
    }


adjustedAge : PersonWithAdjustedAge -> Int -> Int
adjustedAge this threshold =
    if this.age + 1 > threshold then
        this.age + 1

    else
        threshold
