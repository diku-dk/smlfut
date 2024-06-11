use "test.sml";

fun main () =
  let
    val cfg = futhark_context_config_new ()
    val ctx = futhark_context_new cfg
    val () = print "hello, world!!!\n"
    val () = futhark_context_free ctx
    val () = futhark_context_config_free cfg
    val w = Foreign.Memory.alloca (0w8, fn p =>
      let val _ = futhark_entry_main (ctx, p, 10)
      in Foreign.Memory.get64 (p, 0w0)
      end)
    val () = print (Int.toString (Word64.toInt w) ^ "\n")
  in
    ()
  end
  handle e => print ("Unhandled exception:\n" ^ PolyML.makestring e ^ "\n")
