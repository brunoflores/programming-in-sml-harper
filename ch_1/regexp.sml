(* Signature REGEXP describes a module that implements
   regular expressions. *)
signature REGEXP = sig

  (* AST of our regular expressions. *)
  datatype regexp =
      Zero
    | One
    | Char of char
    | Plus of regexp * regexp
    | Times of regexp * regexp
    | Star of regexp

  exception SyntaxError of string

  val parse : string -> regexp

  (* In general there are many strings that parse to the same regular
     expressions; the unparser generally tries to choose one that is easiest
     to read. *)
  val format : regexp -> string
end

(* Signature MATCHER describes a module that implements
   a matcher for a given notion of regular expression. *)
signature MATCHER = sig

  (* Depends on a given implementation of regular expressions. *)
  structure RegExp : REGEXP

  val accepts : RegExp.regexp -> string -> bool
end

(* An implementation of regular expressions. *)
structure RegExp :> REGEXP = struct

  datatype token =
      AtSign
    | Percent
    | Literal of char
    | PlusSign
    | TimesSign
    | Asterisk
    | LParen
    | RParen

  exception LexicalError

  fun tokenize
      nil = nil
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

  datatype regexp =
      Zero
    | One
    | Char of char
    | Plus of regexp * regexp
    | Times of regexp * regexp
    | Star of regexp

  exception SyntaxError of string

  (* Simple recursive-descent parser that dispatches on the head
     of the token list to determine how to proceed. *)
  fun parse_exp ts =
    let val (r, ts') = parse_term ts in
      case ts' of
          (PlusSign :: ts'') =>
            let val (r', ts''') = parse_exp ts'' in
              (Plus (r, r'), ts''')
            end
        | _ => (r, ts')
    end

  and parse_term ts =
    let val (r, ts') = parse_factor ts in
      case ts' of
	  (TimesSign :: ts'') =>
            let val (r', ts''') = parse_term ts'' in
              (Times (r, r'), ts''')
            end
        | _ => (r, ts')
    end

  and parse_factor ts =
    let val (r, ts') = parse_atom ts in
      case ts' of
          (Asterisk :: ts'') => (Star r, ts'')
        | _ => (r, ts')
    end

  and parse_atom
      nil = raise SyntaxError ("factor expected")
    | parse_atom (AtSign :: ts) = (Zero, ts)
    | parse_atom (Percent :: ts) = (One, ts)
    | parse_atom ((Literal c) :: ts) = (Char c, ts)
    | parse_atom (LParen :: ts) = (
        let val (r, ts') = parse_exp ts in
          case ts' of
              nil => raise SyntaxError ("right-parenthesis expected")
            | (RParen :: ts'') => (r, ts'')
            | _ => raise SyntaxError ("right-parenthesis expected")
        end)
    | parse_atom _ = raise SyntaxError ("not an atom")

  (* The parser takes a string, "explodes" is into a list of characters,
     transforms the character list into a list of "tokens", and finally
     parses the resulting list of tokens to obtain its abstract syntax. *)
  fun parse s =
    let val (r, ts) = parse_exp (tokenize (String.explode s)) in
      case ts of
          nil => r
        | _ => raise SyntaxError "unexpected input"
    end
    handle LexicalError => raise SyntaxError "illegal input"

  fun format_exp
      Zero = [#"@"]
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

functor Matcher (structure RegExp : REGEXP) :> MATCHER = struct

  (* Use a given implementation. *)
  structure RegExp = RegExp
  open RegExp

  fun match_is
      Zero _ _ = false
    | match_is One cs k = k cs
    | match_is (Char c) nil _ = false
    | match_is (Char c) (c' :: cs) k = (c = c') andalso (k cs)
    | match_is (Plus (r1, r2)) cs k =
      (match_is r1 cs k) orelse (match_is r2 cs k)
    | match_is (Times (r1, r2)) cs k =
      match_is r1 cs (fn cs' => match_is r2 cs' k)
    | match_is (r as Star r1) cs k =
      (k cs) orelse match_is r1 cs (fn cs' => match_is r cs' k)

  (* The matcher is generalised to one that checks whether some initial
     segment of a string matches a given regular expression, then
     passes the remaining final segment to a continuation [k], a function
     that determines what to do after the initial segment has been
     successfully matched.

     An initial continuation checks whether the final segment is empty. *)
  fun accepts regexp string =
    let val initial_k = (fn nil => true | _ => false) in
      match_is regexp (String.explode string) initial_k
    end
end

(* Instantiate. *)
structure Matcher = Matcher (structure RegExp = RegExp)
