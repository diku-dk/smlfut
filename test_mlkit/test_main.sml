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

fun test ctx name f =
  f ctx
  handle Fail s => print (name ^ " failed: " ^ s ^ "\n")

fun test_i32 ctx =
  let
    val arr_in =
      Futhark.Int32Array1.new ctx
        (Int32ArraySlice.full (Int32Array.fromList [1, 2, 3])) 3
    val arr_out = Futhark.Entry.array_i32 ctx arr_in
    val arr_sml = Futhark.Int32Array1.values arr_out
    val () = Futhark.Context.sync ctx
    val () = Futhark.Int32Array1.free arr_in
    val () = Futhark.Int32Array1.free arr_out
  in
    if Int32ArrayTest.toList arr_sml <> [3, 4, 5, 1, 2, 3] then
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

    Futhark.Context.free ctx
  end
