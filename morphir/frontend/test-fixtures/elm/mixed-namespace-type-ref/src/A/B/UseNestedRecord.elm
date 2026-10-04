module A.B.UseNestedRecord exposing (increment)

import A.B.C.NestedRecord exposing (NestedRecord)

increment : NestedRecord -> Int
increment record =
    record.value + 1
