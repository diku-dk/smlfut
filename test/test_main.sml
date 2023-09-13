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
    val arr_in = Futhark.new_1d_i32 ctx (Int32Array.fromList [1, 2, 3]) 3
    val arr_out = Futhark.entry_array_i32 ctx arr_in
    val arr_sml = Futhark.values_1d_i32 arr_out
  in
    if Int32ArrayTest.toList arr_sml <> [3, 4, 5, 1, 2, 3] then
      raise Fail "Unexpected result"
    else
      ()
  end


fun test_f64 ctx =
  let
    val arr_in = Futhark.new_1d_f64 ctx (Real64Array.fromList [1.0, 2.0, 3.0]) 3
    val arr_out = Futhark.entry_array_f64 ctx arr_in
    val arr_sml = Futhark.values_1d_f64 arr_out
    val expected = Real64Array.fromList [3.0, 4.0, 5.0, 1.0, 2.0, 3.0]
  in
    if not (Real64ArrayTest.equal arr_sml expected) then
      raise Fail "Unexpected result"
    else
      ()
  end

fun test_transpose ctx =
  let
    val arr_in =
      Futhark.new_2d_i32 ctx (Int32Array.fromList [1, 2, 3, 4, 5, 6]) (2, 3)
    val arr_out = Futhark.entry_transpose_i32 ctx arr_in
    val arr_sml = Futhark.values_2d_i32 arr_out
  in
    if Int32ArrayTest.toList arr_sml <> [1, 4, 2, 5, 3, 6] then
      raise Fail "Unexpected result."
    else
      ()
  end

fun test_fails ctx =
  (Futhark.entry_fails ctx 0; raise Fail "Should have failed.")
  handle Futhark.error e =>
    if String.isPrefix "Error: division by zero" e then ()
    else raise Fail ("Got unexpected error: " ^ e)

val () =
  let
    val ctx = Futhark.ctx_new Futhark.default_cfg
    val x = Futhark.entry_main ctx 0w123
  in
    test_i32 ctx;

    test ctx "test_transpose" test_transpose;

    test ctx "text_f64" test_f64;

    test ctx "test_fails" test_fails;

    Futhark.ctx_free ctx
  end
