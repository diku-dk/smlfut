(* Based on a Haskell implementation by Sven Panne.  Many comments are
taken directly from his implementation.  Some changes to make it more
useful in SML. *)

structure GetOpt :> GETOPT =
struct

  (* Utility functions. *)

  fun curry f x y = f (x, y)
  val unlines = concat o map (fn s => s ^ "\n")
  val lines = String.tokens (fn c => c = #"\n")
  fun replicate 0 x = []
    | replicate n x =
        x :: replicate (n - 1) x
  fun unzip3 [] = ([], [], [])
    | unzip3 ((a, b, c) :: abcs) =
        let val (as_, bs, cs) = unzip3 abcs
        in (a :: as_, b :: bs, c :: cs)
        end
  fun flushLeft n s =
    s ^ String.implode (replicate (n - size s) #" ")
  fun concatMap f = List.concat o map f
  fun zipWith3 f (x :: xs) (y :: ys) (z :: sz) =
        f x y z :: zipWith3 f xs ys sz
    | zipWith3 _ _ _ _ = []
  fun splitOn c s =
    let
      val n = size s
      fun loop i =
        if i = n then
          (s, "")
        else if String.sub (s, i) = c then
          (String.substring (s, 0, i), String.substring (s, i, n - i))
        else
          loop (i+1)
    in
      loop 0
    end

  (* Option definitions. *)

  datatype 'a arg_order =
    REQUIRE_ORDER
  | PERMUTE
  | RETURN_IN_ORDER of string -> 'a

  datatype 'a arg_descr =
    NO_ARG of (unit -> 'a)
  | REQ_ARG of (string -> 'a) * string
  | OPT_ARG of (string option -> 'a) * string

  type 'a opt_descr =
    {short: char list, long: string list, arg: 'a arg_descr, desc: string}

  (* Printing help text. *)

  fun fmtShort (NO_ARG _) so = "-" ^ str so
    | fmtShort (REQ_ARG (_, ad)) so =
        "-" ^ str so ^ " " ^ ad
    | fmtShort (OPT_ARG (_, ad)) so =
        "-" ^ str so ^ "[" ^ ad ^ "]"

  fun fmtLong (NO_ARG _) lo = "--" ^ lo
    | fmtLong (REQ_ARG (_, ad)) lo =
        "--" ^ lo ^ "=" ^ ad
    | fmtLong (OPT_ARG (_, ad)) lo =
        "--" ^ lo ^ "[=" ^ ad ^ "]"

  fun fmtOpt ({short=sos, long=los, arg=ad, desc}) =
    let
      fun sepBy _ [] = ""
        | sepBy _ [x] = x
        | sepBy ch (x :: xs) =
            x ^ str ch ^ " " ^ sepBy ch xs
      val sosFmt = sepBy #"," (map (fmtShort ad) sos)
      val losFmt = sepBy #"," (map (fmtLong ad) los)
    in
      case lines desc of
        [] => [(sosFmt, losFmt, "")]
      | (d :: ds) => (sosFmt, losFmt, d) :: map (fn d' => ("", "", d')) ds
    end

  fun usage optDescr =
    let
      fun paste x y z =
        "  " ^ x ^ "  " ^ y ^ "  " ^ z
      fun sameLen xs =
        map (flushLeft ((foldl Int.max 0 o map size) xs)) xs
      val (ss, ls, ds) = unzip3 (concatMap fmtOpt optDescr)
      val table = zipWith3 paste (sameLen ss) (sameLen ls) ds
    in
      unlines table
    end

  (* Actual parsing. *)

  (* Kind of cmd line arg (internal use only). *)
  datatype 'a opt_kind =
    OPT of 'a (* an option *)
  | UNREQ_OPT of string (* an un-recognized option *)
  | NON_OPT of string (* a non-option *)
  | END_OF_OPTS (* end-of-options marker (i.e. "--") *)
  | OPT_ERR of string (* something went wrong... *)


  fun errAmbig ods optStr =
    OPT_ERR ("Option '" ^ optStr ^ "' is ambiguous; could be one of:\n\n" ^
             usage ods)

  fun errReq d optStr =
    OPT_ERR ("Option '" ^ optStr ^ "' requires an argument " ^ d ^ "\n")

  fun errUnrec optStr = "Unrecognized option '" ^ optStr ^ "'.\n"

  fun errNoArg optStr =
    OPT_ERR ("Option '" ^ optStr ^ "' doesn't allow an argument\n")

  fun longOpt ls rs optDescr =
    let
        val (opt, arg) = splitOn #"=" ls
      fun getWith p =
        List.filter (fn o_ => List.find (p opt) (#long o_) <> NONE)
          optDescr
      val exact = getWith (curry (op=))
      val options = if null exact then getWith String.isPrefix else exact
      val ads = map (fn o_ => #arg o_) options
      val optStr = "--" ^ opt
      fun long (_ :: _ :: _) _ rest =
            (errAmbig options optStr, rest)
        | long [NO_ARG f] [] rest = (OPT (f ()), rest)
        | long [NO_ARG _] (#"=" :: _) rest = (errNoArg optStr, rest)
        | long [REQ_ARG (_, d)] [] [] =
            (errReq d optStr, [])
        | long [REQ_ARG (f, _)] [] (r :: rest) =
            (OPT (f r), rest)
        | long [REQ_ARG (f, _)] (#"=" :: xs) rest =
            (OPT (f (implode xs)), rest)
        | long [OPT_ARG (f, _)] [] rest =
            (OPT (f NONE), rest)
        | long [OPT_ARG (f, _)] (#"=" :: xs) rest =
            (OPT (f (SOME (implode xs))), rest)
        | long _ _ rest =
            (UNREQ_OPT ("--" ^ ls), rest)
    in
      long ads (explode arg) rs
    end

  fun shortOpt y ys rs optDescr =
    let
      val options =
        List.filter
          (fn o_ => isSome (List.find (fn c => c = y) (#short o_)))
          optDescr
      val ads = map (fn o_ => #arg o_) options
      val optStr = "-" ^ str y

      fun short (_ :: _ :: _) _ rest =
            (errAmbig options optStr, rest)
        | short (NO_ARG f :: _) [] rest = (OPT (f ()), rest)
        | short (NO_ARG f :: _) xs rest =
            (OPT (f ()), ("-" ^ implode xs) :: rest)
        | short (REQ_ARG (_, d) :: _) [] [] =
            (errReq d optStr, [])
        | short (REQ_ARG (f, _) :: _) [] (r :: rest) =
            (OPT (f r), rest)
        | short (REQ_ARG (f, _) :: _) xs rest =
            (OPT (f (implode xs)), rest)
        | short (OPT_ARG (f, _) :: _) [] rest =
            (OPT (f NONE), rest)
        | short (OPT_ARG (f, _) :: _) xs rest =
            (OPT (f (SOME (implode xs))), rest)
        | short [] [] rest = (UNREQ_OPT optStr, rest)
        | short [] xs rest =
            (UNREQ_OPT optStr, ("-" ^ implode xs) :: rest)
    in
      short ads (explode ys) rs
    end


  (* Take a look at the next cmd line arg and decide what to do with it. *)
  fun getNext "--" rest _ = (END_OF_OPTS, rest)
    | getNext s rest optDescr =
        if String.isPrefix "--" s then
          longOpt (String.extract (s, 2, NONE)) rest optDescr
        else if String.isPrefix "-" s then
          shortOpt (String.sub (s, 1)) (String.extract (s, 2, NONE)) rest
            optDescr
        else
          (NON_OPT s, rest)

  fun getopt' _ _ [] = ([], [], [], [])
    | getopt' ordering optDescr (arg :: args) =
        let
            val (opt, rest) = getNext arg args optDescr
          val (os, xs, us, es) = getopt' ordering optDescr rest
          fun procNextOpt (OPT opt) _ = (opt :: os, xs, us, es)
            | procNextOpt (UNREQ_OPT u) _ = (os, xs, u :: us, es)
            | procNextOpt (NON_OPT x) REQUIRE_ORDER = ([], x :: rest, [], [])
            | procNextOpt (NON_OPT x) PERMUTE = (os, x :: xs, us, es)
            | procNextOpt (NON_OPT x) (RETURN_IN_ORDER f) =
                (f x :: os, xs, us, es)
            | procNextOpt END_OF_OPTS REQUIRE_ORDER = ([], rest, [], [])
            | procNextOpt END_OF_OPTS PERMUTE = ([], rest, [], [])
            | procNextOpt END_OF_OPTS (RETURN_IN_ORDER f) =
                (map f rest, [], [], [])
            | procNextOpt (OPT_ERR e) _ = (os, xs, us, e :: es)
        in
          procNextOpt opt ordering
        end

  fun getopt ordering optDescr args =
    let val (os, xs, us, es) = getopt' ordering optDescr args
    in (os, xs, es @ map errUnrec us)
    end

end
