module Arithmetic.PersonAgePlusOne exposing (agePlusOne)

import Arithmetic.PersonAgePlusOneRecord exposing (PersonAgePlusOneRecord)

agePlusOne : PersonAgePlusOneRecord -> Int
agePlusOne person =
    person.age + 1
