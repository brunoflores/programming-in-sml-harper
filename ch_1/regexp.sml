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

  fun tokenize nil = nil
    | tokenize (#"+" :: cs) = (PlusSign :: tokenize cs)
    | tokenize (#"." :: cs) = (TimesSign :: tokenize cs)
    | tokenize (#"*" :: cs) = (Asterisk :: tokenize cs)
    | tokenize (#"(" :: cs) = (LParen :: tokenize cs)
    | tokenize (#")" :: cs) = (RParen :: tokenize cs)
    | tokenize (#"@" :: cs) = (AtSign :: tokenize cs)
    | tokenize (#"%" :: cs) = (Percent :: tokenize cs)
    | tokenize (#"\\" :: c :: cs) = Literal c :: tokenize cs
    | tokenize (#"\\" :: nil) = raise LexicalError
    | tokenize (#" " :: cs) = tokenize cs
    | tokenize (c :: cs) = Literal c :: tokenize cs

  datatype regexp = Zero
                  | One
                  | Char of char
                  | Plus of regexp * regexp
                  | Times of regexp * regexp
                  | Star of regexp

  exception SyntaxError of string

  (* Simple recursive-descent parser that dispatches on the head
  *  of the token list to determine how to proceed.
  *  Implemented as four mutually recursive functions.
  *)
  fun parse_exp ts =
    let val (r, ts') = parse_term ts in
      case ts' of (PlusSign :: ts'') =>
          let val (r', ts''') = parse_exp ts' in
            (Plus (r, r'), ts''')
          end
        | _ => (r, ts')
    end

  and parse_term ts =
    let val (r, ts') = parse_factor ts in
      case ts' of (TimesSign :: ts'') =>
          let val (r', ts''') = parse_term ts'' in
            (Times (r, r'), ts''')
          end
        | _ => (r, ts')
    end

  and parse_factor ts =
    let val (r, ts') = parse_atom ts in
      case ts' of (Asterisk :: ts'') =>
          (Star r, ts'')
        | _ => (r, ts')
    end

  and parse_atom nil = raise SyntaxError ("factor expected\n")
         | parse_atom (AtSign :: ts) = (Zero, ts)
         | parse_atom (Percent :: ts) = (One, ts)
         | parse_atom ((Literal c) :: ts) = (Char c, ts)
         | parse_atom (LParen :: ts) = (
           let val (r, ts') = parse_exp ts in
             case ts' of nil => raise SyntaxError ("right-parenthesis expected\n")
                | (RParen :: ts'') => (r, ts'')
                | _ => raise SyntaxError ("right-parenthesis expected\n")
           end)
         | parse_atom _ = raise SyntaxError ("not an atom\n")

  (* The parser takes a string, "explodes" is into a list of characters,
  *  transforms the character list into a list of "tokens", and finally
  *  parses the resulting list of tokens to obtain its abstract syntax.
  *)
  fun parse s =
    let val (r, ts) = parse_exp (tokenize (String.explode s)) in
      case ts of nil => r
         | _ => raise SyntaxError "unexpected input\n"
    end
    handle LexicalError => raise SyntaxError "illegal input\n"

  fun format_exp Zero = [#"@"]
    | format_exp One = [#"%"]
    | format_exp (Char c) = [c]
    | format_exp (Plus (r1, r2)) =
      let val s1 = format_exp r1
          val s2 = format_exp r2 in
        [#"("] @ s1 @ [#"+"] @ s2 @ [#")"]
      end
    | format_exp (Times (r1, r2))=
      let val s1 = format_exp r1
          val s2 = format_exp r2 in
        s1 @ [#"*"] @ s2
      end
    | format_exp (Star r) =
      let val s = format_exp r in
        [#"("] @ s @ [#")"] @ [#"*"]
      end

  fun format r = String.implode (format_exp r)

end
