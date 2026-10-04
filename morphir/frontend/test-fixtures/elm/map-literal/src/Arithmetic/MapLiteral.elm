module Arithmetic.MapLiteral exposing (values)

import Dict exposing (Dict)

values : Dict String Int
values =
    Dict.fromList [ ( "a", 1 ), ( "b", 2 ) ]
