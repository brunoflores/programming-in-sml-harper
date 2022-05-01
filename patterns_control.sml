fun reduce (unit, opn, nil) = unit
  | reduce (unit, opn, h :: t) = opn (h, reduce (unit, opn, t))

val add_up = fn l => reduce (0, op +, l)
fun mul_up l = reduce (1, op *, l)
