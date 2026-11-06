module Arithmetic.PersonWithAgeCheck exposing (PersonWithAgeCheck, isAtLeast)

type alias PersonWithAgeCheck =
    { age : Int
    }


isAtLeast : PersonWithAgeCheck -> Int -> Bool
isAtLeast this threshold =
    this.age >= threshold
