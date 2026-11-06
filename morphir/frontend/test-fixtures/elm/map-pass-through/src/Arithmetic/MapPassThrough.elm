module Arithmetic.MapPassThrough exposing (keep)

import Dict exposing (Dict)

keep : Dict String Int -> Dict String Int
keep values =
    values
