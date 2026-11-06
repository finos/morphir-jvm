package a.b

import a.b.c.NestedRecord

object UseNestedRecord:
  def increment(record: NestedRecord): Int =
    record.value + 1
