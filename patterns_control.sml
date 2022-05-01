(* The function [reduce] abstracts the pattern of defining
*  a function by induction on the structure of a list.
*)
fun reduce (unit, opn, l) =
  let fun red nil = unit
        | red (h :: t) = opn (h, red t) in
    red l
  end

val add_up = fn l => reduce (0, op +, l)
fun mul_up l = reduce (1, op *, l)
