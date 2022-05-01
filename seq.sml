signature SEQUENCE = sig

  type 'a seq = int -> 'a

  (* constant sequence *)
  val constantly : 'a -> 'a seq

  (* alternating values *)
  val alternately : 'a * 'a -> 'a seq

  (* insert at front *)
  val insert : 'a * 'a seq -> 'a seq

  val map : ('a -> 'b) -> 'a seq -> 'b seq

  val zip : 'a seq * 'b seq -> ('a * 'b) seq

  val unzip : ('a * 'b) seq -> 'a seq * 'b seq

  (* fair merge *)
  val merge : ('a seq * 'a seq) -> 'a seq

  val stretch : int -> 'a seq -> 'a seq

  val shrink : int -> 'a seq -> 'a seq

  val take : int -> 'a seq -> 'a list

  val drop : int -> 'a seq -> 'a seq

  val shift : 'a seq -> 'a seq

  val loopback : ('a seq -> 'a seq) -> 'a seq

end

structure Sequence :> SEQUENCE = struct

  type 'a seq = int -> 'a

  fun constantly c _ = c

  fun alternately (c, d) n = 
    if n mod 2 = 0 then c else d

  fun insert (x, _) 0 = x
    | insert (_, s) n = s (n-1)

  fun map f s = f o s

  fun zip (s1, s2) n = (s1 n, s2 n)

  fun unzip (s : ('a * 'b) seq) = 
    (map #1 s, map #2 s)

  fun merge (s1, s2) n =
    (if n mod 2 = 0 then s1 else s2) (n div 2)

  fun stretch k s n = s (n div k)

  fun shrink k s n = s (n*k)

  fun drop k s n = s (n+k)

  fun shift s = drop 1 s

  fun take 0 _ = nil
    | take n s = s 0 :: take (n-1) (shift s)

  fun loopback loop n = loop (loopback loop) n

end

open Sequence

val evens : int seq = fn n => 2*n
val odds : int seq = fn n => 2*n+1
val nats : int seq = merge (evens, odds)

fun fibs n =
  (insert
    (1, insert
      (1, map (op +)
              (zip (drop 1 fibs, fibs)))))(n)

(* We may apply the sequence package to build
*  an implementation of digital circuits.
*  Wires are represented as sequence of levels.
*)

datatype level = High | Low | Undef

type wire = level seq
type pair = (level * level) seq

val Zero : wire = constantly Low
val One : wire = constantly High

(* clock pulse with given duration of each pulse *)
fun clock (freq : int) : wire =
  stretch freq (alternately (Low, High))

(* apply two functions in parallel *)
infixr **;
fun (f ** g) (x, y) = (f x, g y)

(* hardware logical and *)
fun logical_and (Low, _) = Low
  | logical_and (_, Low) = Low
  | logical_and (High, High) = High
  | logical_and _ = Undef

fun logical_not Undef = Undef
  | logical_not High = Low
  | logical_not Low = High

fun logical_nop l = l

(* a nor b = not a and not b *)
val logical_nor = logical_and o (logical_not ** logical_not)

type unary_gate = wire -> wire
type binary_gate = pair -> wire

(* logic gate with unit propagation delay *)
fun gate f w 0 = Undef
  | gate f w i = f (w (i-1))

val delay : unary_gate = gate logical_nop
val inverter : unary_gate = gate logical_not
val nor_gate : binary_gate = gate logical_nor
