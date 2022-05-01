signature ORDERED = sig
  type t
  val lt : t * t -> bool
  val eq : t * t -> bool
end

structure LexString : ORDERED = struct
  type t = string
  fun lt s = String.compare s = LESS
  fun eq s = String.compare s = EQUAL
end

structure LessInt : ORDERED = struct
  type t = int
  fun lt n = Int.compare n = LESS
  fun eq n = Int.compare n = EQUAL
end

structure DivInt : ORDERED = struct
  type t = int
  fun lt (m, n) = (n mod m = 0)
  fun eq (m, n) = lt (m, n) andalso lt (n, m)
end

signature DICT = sig
  structure Key : ORDERED
  type 'a dict
  val empty : 'a dict
  val insert : 'a dict * Key.t * 'a -> 'a dict
  val lookup : 'a dict * Key.t -> 'a option
end

signature STRING_DICT = DICT where type Key.t = string
signature INT_DICT = DICT where type Key.t = int

functor DictFun (structure K : ORDERED) :> DICT where type Key.t = K.t = struct

  structure Key : ORDERED = K

  datatype 'a dict = Empty
                   | Node of 'a dict * Key.t * 'a * 'a dict

  val empty = Empty

  fun insert (None, k, v) = Node (Empty, k, v, Empty)

  fun lookup (Empty, _) = NONE
    | lookup (Node (dl, l, v, dr), k) =
      if Key.lt (k, l) then lookup (dl, k)
      else if Key.lt (l, k) then lookup (dr, k)
      else SOME v

end

structure LtIntDict = DictFun (structure K = LessInt)
structure LexStringDict = DictFun (structure K = LexString)
structure DivIntDict = DictFun (structure K = DivInt)
