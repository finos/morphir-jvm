module Arithmetic.PersonWithMethods exposing (PersonWithMethods, isAdult, nextAge)

type alias PersonWithMethods =
    { age : Int
    }


isAdult : PersonWithMethods -> Bool
isAdult this =
    this.age >= 18


nextAge : PersonWithMethods -> Int
nextAge this =
    this.age + 1
