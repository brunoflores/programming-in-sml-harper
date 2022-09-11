structure Main = struct

  fun main(arg0, argv) = (
    print "hello\n";
    OS.Process.success
  )

  val _ = SMLofNJ.exportFn("main", main)

end
