infix xor

fun false xor true  = true
  | true  xor false = true
  | _     xor _     = false
