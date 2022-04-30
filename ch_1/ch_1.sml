use "regexp.sml";

structure CH1 = struct
  fun ch_1 _ = 0

  fun greet name = name
  fun main (_, f) = let val _ =  ch_1 () in 0 end
end

val regexp = Matcher.RegExp.parse "(a+b)*"
val matches = Matcher.accepts regexp
val ex1 = matches "aabba"
val ex2 = matches "abac"
