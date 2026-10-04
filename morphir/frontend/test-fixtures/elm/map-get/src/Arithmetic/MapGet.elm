module Arithmetic.MapGet exposing (find)

import Dict exposing (Dict)

find : String -> Dict String Int -> Maybe Int
find key values =
    Dict.get key values
