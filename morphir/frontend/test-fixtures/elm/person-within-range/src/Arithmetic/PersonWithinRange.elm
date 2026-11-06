module Arithmetic.PersonWithinRange exposing (PersonWithinRange, adjustedAge)

type alias PersonWithinRange =
    { age : Int
    }


adjustedAge : PersonWithinRange -> Int -> Int -> Int
adjustedAge this lowerBound upperBound =
    if this.age < lowerBound then
        lowerBound

    else if this.age > upperBound then
        upperBound

    else
        this.age
