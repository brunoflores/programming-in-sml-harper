exception Factorial

local
  fun fact 0 = 1
    | fact n = n * fact (n-1)
in
  fun checked_factorial n =
    if n >= 0 then
      fact n
    else
      raise Factorial
end

structure Factorial = struct

  fun read_integer _ = 
    case TextIO.inputLine TextIO.stdIn of
         SOME s => s
       | NONE => "0"

  fun main (prog_name, args) =
    let val input = (case Int.fromString (read_integer ()) of
                         SOME n => n
                       | NONE => 0)
        val result = checked_factorial input in
      print (Int.toString result); 
      print "\n";
      0
    end
    handle Factorial => (print "out of range\n"; 1)

end
