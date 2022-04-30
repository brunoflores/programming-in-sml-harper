(* Signature REGEXP describes a module that implements
*  regular expressions.
*)
signature REGEXP = sig

  datatype regexp = Zero
                  | One
                  | Char of char
                  | Plus of regexp * regexp
                  | Times of regexp * regexp
                  | Star of regexp

  exception SyntaxError of string

  (* Parser. *)
  val parse : string -> regexp

  (* Unparser. *)
  val format : regexp -> string
end

(* Signature MATCHER describes a module that implements
*  a matcher for a given notion of regular expression.
*)
signature MATCHER = sig

  (* Depends on a given implementation of regular expressions. *)
  structure RegExp : REGEXP

  val match : RegExp.regexp -> string -> bool

end

structure RegExp :> REGEXP = struct

  datatype token = AtSign
                 | Percent
                 | Literal of char
                 | PlusSign
                 | TimesSign
                 | Asterisk
                 | LParen
                 | RParen

  exception LexicalError

  datatype regexp = Zero
                  | One
                  | Char of char
                  | Plus of regexp * regexp
                  | Times of regexp * regexp
                  | Star of regexp

  exception SyntaxError of string

  fun parse _ = Zero

  fun format _ = ""

end
