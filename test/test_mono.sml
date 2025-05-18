(* Generic tests for backends that use monomorphic arrays. *)

structure Futhark = test
functor ArrayTest (structure A: MONO_ARRAY; val eq: A.elem * A.elem -> bool) :>
sig
  val equal: A.array -> A.array -> bool
  val toList: A.array -> A.elem list
end =
struct
  fun equal x y =
    let
      val n = A.length x
      val m = A.length y
      fun loop i =
        if i = n then true
        else eq (A.sub (x, i), A.sub (y, i)) andalso loop (i + 1)
    in
      A.length x = A.length y andalso loop 0
    end
  fun toList x =
    let val n = A.length x
    in List.tabulate (n, fn i => A.sub (x, i))
    end
end

structure Int32ArrayTest = ArrayTest (structure A = Int32Array; val eq = op=)
structure Real64ArrayTest =
  ArrayTest (structure A = Real64Array; val eq = Real64.==)

fun printArray app toString arr =
  (print "["; app (fn x => print (toString x ^ " ")) arr; print "]\n")

fun test_i32 ctx =
  let
    val arr_in =
      Futhark.Int32Array1.new ctx
        (Int32ArraySlice.full (Int32Array.fromList [1, 2, 3])) 3
    val x = Futhark.Int32Array1.index arr_in 1
    val arr_out = Futhark.Entry.array_i32 ctx arr_in
    val arr_sml = Futhark.Int32Array1.values arr_out
    val () = Futhark.Context.sync ctx
    val () = Futhark.Int32Array1.free arr_in
    val () = Futhark.Int32Array1.free arr_out
  in
    if x <> 2 orelse Int32ArrayTest.toList arr_sml <> [3, 4, 5, 1, 2, 3] then
      raise Fail "Unexpected result"
    else
      ()
  end

fun test_f64 ctx =
  let
    val arr_in =
      Futhark.Real64Array1.new ctx
        (Real64ArraySlice.full (Real64Array.fromList [1.0, 2.0, 3.0])) 3
    val arr_out = Futhark.Entry.array_f64 ctx arr_in
    val arr_sml = Futhark.Real64Array1.values arr_out
    val () = Futhark.Context.sync ctx
    val expected = Real64Array.fromList [3.0, 4.0, 5.0, 1.0, 2.0, 3.0]
    val () = Futhark.Real64Array1.free arr_in
    val () = Futhark.Real64Array1.free arr_out
  in
    if not (Real64ArrayTest.equal arr_sml expected) then
      raise Fail "Unexpected result"
    else
      ()
  end

fun test_transpose ctx =
  let
    val arr_in =
      Futhark.Int32Array2.new ctx
        (Int32ArraySlice.full (Int32Array.fromList [1, 2, 3, 4, 5, 6])) (2, 3)
    val arr_out = Futhark.Entry.transpose_i32 ctx arr_in
    val arr_sml = Futhark.Int32Array2.values arr_out
    val () = Futhark.Context.sync ctx
    val () = Futhark.Int32Array2.free arr_in
    val () = Futhark.Int32Array2.free arr_out
  in
    if Int32ArrayTest.toList arr_sml <> [1, 4, 2, 5, 3, 6] then
      raise Fail "Unexpected result."
    else
      ()
  end

fun test_fails ctx =
  ( Futhark.Entry.fails ctx 0
  ; Futhark.Context.sync ctx
  ; raise Fail "Should have failed."
  )
  handle Futhark.Error e =>
    if String.isPrefix "Error: division by zero" e then ()
    else raise Fail ("Got unexpected error: " ^ e)

fun test_record ctx =
  let
    val record = Futhark.Opaque.record.new ctx {a = 2, b = true}
    val {a, b} = Futhark.Opaque.record.values record
  in
    if a <> 2 orelse b <> true then raise Fail "Unexpected result." else ();
    Futhark.Opaque.record.free record
  end

fun test_store ctx =
  let
    val record = Futhark.Opaque.record.new ctx {a = 2, b = true}
    val bytes = Futhark.Opaque.record.store record
    val () = Futhark.Opaque.record.free record
    val record = Futhark.Opaque.record.restore ctx (Word8ArraySlice.full bytes)
    val {a, b} = Futhark.Opaque.record.values record
  in
    if a <> 2 orelse b <> true then raise Fail "Unexpected result." else ();
    Futhark.Opaque.record.free record;
    ( Futhark.Opaque.record.free record
    ; raise Fail "Allowed to use freed record"
    )
    handle Futhark.Free => ()
  end

fun test_sum ctx =
  let
    val sum =
      Futhark.Opaque.sum_opaque.new ctx (Futhark.Opaque.sum_opaque.foo 2)
    val n = Futhark.Entry.sum_opaque_size ctx sum
    val sum_next = Futhark.Entry.sum_opaque_rot ctx sum
    val m = Futhark.Entry.sum_opaque_size ctx sum_next
  in
    if n <> 0 then raise Fail ("Unexpected n: " ^ Int.toString n) else ();
    if m <> 10 then raise Fail ("Unexpected m: " ^ Int.toString m) else ();
    (case Futhark.Opaque.sum_opaque.values sum_next of
       Futhark.Opaque.sum_opaque.foo _ => raise Fail "Unexpected foo"
     | Futhark.Opaque.sum_opaque.baz _ => raise Fail "Unexpected baz"
     | Futhark.Opaque.sum_opaque.bar r => Futhark.Opaque.record.free r);
    Futhark.Opaque.sum_opaque.free sum;
    Futhark.Opaque.sum_opaque.free sum_next
  end

fun test_record_array ctx =
  let
    val fs1 =
      Futhark.Int32Array2.new ctx
        (Int32ArraySlice.full (Int32Array.fromList [1, 2, 3, 4, 5, 6])) (3, 2)
    val fs2 =
      Futhark.Int32Array1.new ctx
        (Int32ArraySlice.full (Int32Array.fromList [7, 8, 9])) 3
    val fs3 =
      Futhark.Int32Array1.new ctx
        (Int32ArraySlice.full (Int32Array.fromList [7, 8])) 2
    val () =
      ( Futhark.Opaque.arr_tup2_arr_i32_i32.zip (fs1, fs3)
      ; raise Fail "zip did not raise Size"
      )
      handle Size => ()
    val ra = Futhark.Opaque.arr_tup2_arr_i32_i32.zip (fs1, fs2)
    val () =
      if Futhark.Opaque.arr_tup2_arr_i32_i32.shape ra <> 3 then
        raise Fail "Unexpected shape"
      else
        ()
    val () = Futhark.Int32Array2.free fs1
    val () = Futhark.Int32Array1.free fs2
    val x = Futhark.Opaque.arr_tup2_arr_i32_i32.index ra 1
    val () = Futhark.Opaque.arr_tup2_arr_i32_i32.free ra
  in
    ()
  end

fun test_notice_use_after_free ctx =
  let
    val sum =
      Futhark.Opaque.sum_opaque.new ctx (Futhark.Opaque.sum_opaque.foo 2)
  in
    ( Futhark.Opaque.sum_opaque.free sum
    ; Futhark.Entry.sum_opaque_size ctx sum
    ; raise Fail "Did not notice use-after-free."
    )
    handle Futhark.Free => ()
  end


val status = ref OS.Process.success

fun test ctx name f =
  f ctx
  handle
    Fail s =>
      (print (name ^ " failed: " ^ s ^ "\n"); status := OS.Process.failure)
  | Futhark.Error s =>
      ( print (name ^ " failed (Futhark Error): " ^ s ^ "\n")
      ; status := OS.Process.failure
      )
  | Futhark.Free =>
      ( print (name ^ " failed (use-after-free)\n")
      ; status := OS.Process.failure
      )
  | _ =>
      ( print (name ^ " failed (unexpected exception)\n")
      ; status := OS.Process.failure
      )

val () =
  let
    val ctx = Futhark.Context.new
      (Futhark.Config.cache (SOME "futhark.cache") Futhark.Config.default)
    val x = Futhark.Entry.main ctx 0w123
  in
    test_i32 ctx;

    test ctx "test_transpose" test_transpose;

    test ctx "text_f64" test_f64;

    test ctx "test_fails" test_fails;

    test ctx "test_record" test_record;

    test ctx "test_store" test_store;

    test ctx "test_sum" test_sum;

    test ctx "test_record_array" test_record_array;

    test ctx "test_notice_use_after_free" test_notice_use_after_free;

    Futhark.Context.free ctx;

    ((test_i32 ctx; raise Fail "Allowed to use freed context")
     handle Futhark.Free => ());

    OS.Process.exit (!status)
  end
