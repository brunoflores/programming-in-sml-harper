fun newcounter _ =
  let val state = ref 0
    val counter = 
      (fn _ => let val old = !state in
        state := old + 1; old
      end) : unit -> int in 
    counter
  end

val c1 = newcounter ();
c1 ();
c1 ()
