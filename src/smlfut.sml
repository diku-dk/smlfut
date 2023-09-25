(* FFI stuff *)

(* Invoke named C function with the provided (SML) arguments and return type *)
fun fficall cfun args ret =
  let
    val (arg_es, arg_ts) = ListPair.unzip args
  in
    apply
      (parens
         ("_import \"" ^ cfun ^ "\" public : " ^ tuple_t arg_ts ^ " -> " ^ ret
          ^ ";")) arg_es
  end

val pointer = "MLton.Pointer.t"
val null = "MLton.Pointer.null"

(* Actual logic. *)

fun constituent c =
  Char.isAlphaNum c orelse c = #"'" orelse c = #"_"

fun isValidName s =
  List.all constituent (explode s)

fun checkValidName s =
  if isValidName s then ()
  else raise Fail ("\"" ^ s ^ "\" is an invalid SML identifier.")

fun isPrimType "i8" = SOME "Int8.int"
  | isPrimType "i16" = SOME "Int16.int"
  | isPrimType "i32" = SOME "Int32.int"
  | isPrimType "i64" = SOME "Int64.int"
  | isPrimType "u8" = SOME "Word8.word"
  | isPrimType "u16" = SOME "Word16.word"
  | isPrimType "u32" = SOME "Word32.word"
  | isPrimType "u64" = SOME "Word64.word"
  | isPrimType "f16" = SOME "Real16.real"
  | isPrimType "f32" = SOME "Real32.real"
  | isPrimType "f64" = SOME "Real64.real"
  | isPrimType "bool" = SOME "Bool.bool"
  | isPrimType _ = NONE

fun primTypeToSML t =
  case isPrimType t of
    SOME t' => t'
  | NONE => raise Fail ("Cannot map type to SML: " ^ t)

fun smlArrayModule (info: array_info) =
  case #elemtype info of
    "i8" => "Int8Array"
  | "i16" => "Int16Array"
  | "i32" => "Int32Array"
  | "i64" => "Int64Array"
  | "u8" => "Word8Array"
  | "u16" => "Word16Array"
  | "u32" => "Word32Array"
  | "u64" => "Word64Array"
  | "bool" => "BoolArray"
  | "f16" => "Real16Array"
  | "f32" => "Real32Array"
  | "f64" => "Real64Array"
  | _ =>
      raise Fail
        ("Cannot represent SML array with element type: " ^ #elemtype info)

fun smlArrayType info = smlArrayModule info ^ ".array"

fun futharkArrayStruct (info: array_info) =
  smlArrayModule info ^ Int.toString (#rank info)

fun futharkArrayType (info: array_info) = futharkArrayStruct info ^ ".array"

fun futharkOpaqueStruct name =
  let
    fun escape c =
      if constituent c then c else #"_"
  in
    "opaque_" ^ String.map escape name
  end

fun futharkOpaqueType name = futharkOpaqueStruct name ^ ".t"

fun futharkTypeToSML name (FUTHARK_ARRAY info) = futharkArrayType info
  | futharkTypeToSML name (FUTHARK_OPAQUE info) = futharkOpaqueType name

fun apiType t =
  case isPrimType t of
    SOME t' => t'
  | NONE => pointer

fun typeToSML manifest t =
  case lookupType manifest t of
    SOME t' => futharkTypeToSML t t'
  | NONE => primTypeToSML t

fun blankRef manifest t =
  case lookupType manifest t of
    SOME _ => null
  | NONE =>
      case t of
        "i8" => "Int8.fromInt 0"
      | "i16" => "Int16.fromInt 0"
      | "i32" => "Int32.fromInt 0"
      | "i64" => "Int64.fromInt 0"
      | "u8" => "Word8.fromInt 0"
      | "u16" => "Word16.fromInt 0"
      | "u32" => "Word32.fromInt 0"
      | "u64" => "Word64.fromInt 0"
      | "f16" => "Real16.fromInt 0"
      | "f32" => "Real32.fromInt 0"
      | "f64" => "Real64.fromInt 0"
      | "bool" => "false"
      | _ => raise Fail ("blankRef: " ^ t)

fun generateEntrySpec manifest (name, entry_point {cfun, inputs, outputs}) =
  valspec name ["ctx", tuple_t (map (typeToSML manifest o #type_) inputs)]
    (tuple_t (map (typeToSML manifest o #type_) outputs))

fun mkSum [] = "0"
  | mkSum [x] = x
  | mkSum (x :: xs) = x ^ "+" ^ mkSum xs


fun mkProd [] = "1"
  | mkProd [x] = x
  | mkProd (x :: xs) = x ^ "*" ^ mkProd xs

fun mkSize (info: array_info) v =
  letbind
    [( "shape_c"
     , fficall (#shape (#ops info)) [("ctx", "futhark_context"), (v, pointer)]
         pointer
     )]
    [(mkProd (List.tabulate (#rank info, fn i =>
        apply "Int64.toInt"
          [apply "MLton.Pointer.getInt64" ["shape_c", Int.toString i]])))]

fun mkShape (info: array_info) v =
  letbind
    [( "shape_c"
     , fficall (#shape (#ops info)) [("ctx", "futhark_context"), (v, pointer)]
         pointer
     )]
    [tuple_e (List.tabulate (#rank info, fn i =>
       apply "Int64.toInt"
         [apply "MLton.Pointer.getInt64" ["shape_c", Int.toString i]]))]

(* Extracting the Futhark error string is somewhat tricky, for two reasons:

1) We have to convert it to an SML string.

2) We are responsible for freeing the C string.

Our solution is to allocate an SML string, copy the C string into it,
then free the C string.
 *)

val strlen =
  ["fun strlen (p: MLton.Pointer.t) ="]
  @
  map indent
    [ "let"
    , indent "fun loop i ="
    , indent (indent
        ("if MLton.Pointer.getWord8 (p, i) = 0w0 then i else loop (i+1)"))
    , "in"
    , indent "loop 0"
    , "end"
    ]

val strcpy =
  [ "fun strcpy (p: MLton.Pointer.t) : string ="
  , indent "CharVector.tabulate (strlen p, fn i =>"
  , indent "Char.chr (Word8.toInt (MLton.Pointer.getWord8 (p, i))))"
  ]

val error_check =
  ["local"] @ map indent strlen @ map indent strcpy @ ["in"]
  @
  fundef "get_error" ["ctx"]
    (letbind
       [ ( "p"
         , fficall "futhark_context_get_error" [("ctx", "futhark_context")]
             pointer
         )
       , ("s", "strcpy p")
       , ("()", fficall "free" [("p", pointer)] "unit")
       ] ["s"]) @ ["end"]
  @
  fundef "error_check" ["(err,ctx)"]
    ["if err = 0 then () else raise error (get_error(ctx))"]

fun generateEntryDef manifest (name, ep as entry_point {cfun, inputs, outputs}) =
  let
    fun inpParams i [] = []
      | inpParams i ({name = _, type_, unique = _} :: rest) =
          let
            val v = "inp" ^ Int.toString i
          in
            (case lookupType manifest type_ of
               SOME _ => "(_, " ^ v ^ ")"
             | _ => v) :: inpParams (i + 1) rest
          end
    fun outDecs i [] = []
      | outDecs i ({type_, unique = _} :: rest) =
          ("out" ^ Int.toString i, "ref (" ^ blankRef manifest type_ ^ ")")
          :: outDecs (i + 1) rest
    fun outArgs i [] = []
      | outArgs i (out :: rest) =
          ("out" ^ Int.toString i, apiType (#type_ out) ^ " ref")
          :: outArgs (i + 1) rest
    fun inpArgs i [] = []
      | inpArgs i (inp :: rest) =
          ("inp" ^ Int.toString i, apiType (#type_ inp)) :: inpArgs (i + 1) rest
    fun outRes i [] = []
      | outRes i (out :: rest) =
          let
            val v = "out" ^ Int.toString i
          in
            (case lookupType manifest (#type_ out) of
               SOME _ => tuple_e ["ctx", "!" ^ v]
             | _ => "!" ^ v) :: outRes (i + 1) rest
          end
  in
    fundef name (["{cfg,ctx}", tuple_e (inpParams 0 inputs)])
      (letbind
         (outDecs 0 outputs
          @
          [ ( "ret"
            , fficall cfun
                ([("ctx", "futhark_context")] @ outArgs 0 outputs
                 @ inpArgs 0 inputs) "Int32.int"
            )
          , ("()", "error_check(Int32.toInt ret, ctx)")
          ]) [tuple_e (outRes 0 outputs)])
  end

fun shapeTypeOfRank d =
  (tuple_t o replicate d) "int"

fun origNameComment name =
  ["(* " ^ name ^ " *)"]

fun generateTypeSpec manifest (name, FUTHARK_ARRAY info) =
      origNameComment name
      @
      [ structspec (futharkArrayStruct info) "FUTHARK_ARRAY"
      , "where type ctx = ctx"
      , "  and type shape = " ^ shapeTypeOfRank (#rank info)
      , "  and type Native.array = " ^ smlArrayModule info ^ ".array"
      , "  and type Native.elem = " ^ smlArrayModule info ^ ".elem"
      ]
  | generateTypeSpec manifest (name, FUTHARK_OPAQUE info) =
      case #record info of
        NONE =>
          origNameComment name
          @
          [ structspec (futharkOpaqueStruct name) "FUTHARK_OPAQUE"
          , "where type ctx = ctx"
          ]
      | SOME record =>
          let
            fun fieldType (name, {project, type_}) =
              (name, typeToSML manifest type_)
          in
            origNameComment name
            @
            [ structspec (futharkOpaqueStruct name) "FUTHARK_RECORD"
            , "where type ctx = ctx"
            , "  and type record = " ^ record_t (map fieldType (#fields record))
            ]
          end

fun generateTypeDef manifest
      (name, FUTHARK_ARRAY (info as {ctype, rank, elemtype, ops})) =
      let
        val data_t = smlArrayType info
        val shape =
          if rank = 1 then
            ["Int64.fromInt shape"]
          else
            List.tabulate (rank, fn i =>
              apply "Int64.fromInt" ["#" ^ Int.toString (i + 1) ^ " shape"])
        val shape_args = map (fn x => (x, "Int64.int")) shape
      in
        structdef (futharkArrayStruct info) NONE
          ([ typedef "array" [] (tuple_t ["futhark_context", pointer])
           , typedef "ctx" [] "ctx"
           , typedef "shape" [] (shapeTypeOfRank rank)
           , "structure Native = " ^ smlArrayModule info
           ]
           @
           fundef "new"
             [ "{ctx,cfg}"
             , parens ("data: " ^ data_t)
             , parens ("shape: " ^ shapeTypeOfRank rank)
             ]
             (letbind
                [( "arr"
                 , fficall (#new ops)
                     ([("ctx", "futhark_context"), ("data", data_t)]
                      @ shape_args) pointer
                 )]
                [ "if arr = " ^ null
                , "then raise error (get_error ctx)"
                , "else sync ctx; (ctx, arr)"
                ])
           @
           fundef "free" ["(ctx,data)"]
             [apply "error_check"
                [ (fficall (#free ops)
                     ([("ctx", "futhark_context"), ("data", pointer)]) "int")
                , "ctx"
                ]] @ fundef "shape" ["(ctx,data)"] (mkShape info "data")
           @
           fundef "values" ["(ctx, data)"]
             (letbind
                [ ("n", unlines (mkSize info "data"))
                , ( "out"
                  , apply (smlArrayModule info ^ ".array")
                      ["n", blankRef manifest elemtype]
                  )
                , ( "()"
                  , apply "error_check"
                      [ fficall (#values ops)
                          [ ("ctx", "futhark_context")
                          , ("data", pointer)
                          , ("out", data_t)
                          ] "Int32.int"
                      , "ctx"
                      ]
                  )
                , ("()", apply "sync" ["ctx"])
                ] ["out"]))
      end
  | generateTypeDef manifest (name, FUTHARK_OPAQUE info) =
      let
        val more =
          case #record info of
            NONE => []
          | SOME record =>
              let
                fun getField (name, {project, type_}) =
                  ( name
                  , "let val out = ref " ^ parens (blankRef manifest type_)
                    ^ "in "
                    ^
                    apply "error_check"
                      [ fficall project
                          [ ("ctx", "futhark_context")
                          , ("out", apiType type_ ^ " ref")
                          , ("data", pointer)
                          ] "int"
                      , "ctx"
                      ] ^ "; "
                    ^
                    (case lookupType manifest type_ of
                       SOME _ => tuple_e ["ctx", "!out"]
                     | _ => "!out") ^ " end"
                  )
                fun fieldParam (name, {project, type_}) =
                  ( name
                  , case isPrimType type_ of
                      SOME _ => name
                    | NONE => tuple_e ["_", name]
                  )
                fun fieldArg (name, {project, type_}) = (name, apiType type_)
                fun fieldType (name, {project, type_}) =
                  (name, typeToSML manifest type_)
              in
                [typedef "record" [] (record_t (map fieldType (#fields record)))]
                @
                fundef "values" ["(ctx,data)"]
                  [record_e (map getField (#fields record))]
                @
                fundef "new"
                  ["{cfg,ctx}", record_e (map fieldParam (#fields record))]
                  (letbind [("out", "ref " ^ null)]
                     [ apply "error_check"
                         [ (fficall (#new record)
                              ([ ("ctx", "futhark_context")
                               , ("out", pointer ^ " ref")
                               ] @ map fieldArg (#fields record)) "int")
                         , "ctx"
                         ] ^ ";"
                     , "(ctx,!out)"
                     ])
              end
      in
        structdef (futharkOpaqueStruct name) NONE
          ([ typedef "ctx" [] "ctx"
           , typedef "t" [] (tuple_t ["futhark_context", pointer])
           ]
           @
           fundef "free" ["(ctx,data)"]
             [apply "error_check"
                [ (fficall (#free (#ops info))
                     ([("ctx", "futhark_context"), ("data", pointer)]) "int")
                , "ctx"
                ]] @ more)
      end

val array_signature =
  [ "signature FUTHARK_ARRAY ="
  , "sig"
  , "  type array"
  , "  type ctx"
  , "  type shape"
  , "  structure Native : MONO_ARRAY"
  , "  val new: ctx -> Native.array -> shape -> array"
  , "  val free: array -> unit"
  , "  val shape: array -> shape"
  , "  val values: array -> Native.array"
  , "end"
  ]


val opaque_signature =
  [ "signature FUTHARK_OPAQUE ="
  , "sig"
  , "  type t"
  , "  type ctx"
  , "  val free : t -> unit"
  , "end"
  ]

val record_signature =
  [ "signature FUTHARK_RECORD ="
  , "sig"
  , "  include FUTHARK_OPAQUE"
  , "  type record"
  , "  val values : t -> record"
  , "  val new : ctx -> record -> t"
  , "end"
  ]

(* The manifest does not guarantee anything about the type ordering,
but some types (specifically records) may refer to other types.  It is
important that they are declared before they are used.  Fortunately
there is at least no possibility of cycles. *)
fun orderTypes types =
  let
    fun order _ [] = []
      | order known rs =
          let
            fun isKnown v =
              case isPrimType v of
                SOME _ => true
              | NONE => isSome (List.find (fn x => x = v) known)
            fun usesKnown
                  (name, FUTHARK_OPAQUE {ctype, ops, record = SOME record}) =
                  List.all (isKnown o #type_ o #2) (#fields record)
              | usesKnown _ = true
            val (ok, next) = List.partition usesKnown rs
          in
            ok @ order (map #1 ok) next
          end
  in
    order [] types
  end

fun generate sig_name struct_name
  (manifest as MANIFEST {backend, version, entry_points, types}) =
  let
    val type_cfg = typedef "cfg" []
      (record_t
         [("logging", "bool"), ("debugging", "bool"), ("profiling", "bool")])
    val def_cfg =
      record_e
        [("logging", "false"), ("debugging", "false"), ("profiling", "false")]
    val exn_fut = "exception error of string"
    val entry_specs = map (generateEntrySpec manifest) entry_points
    val entry_defs = map (generateEntryDef manifest) entry_points
    val types = orderTypes types
    val type_specs =
      (List.concat o intersperse [""] o map (generateTypeSpec manifest)) types
    val type_defs =
      (List.concat o intersperse [""] o map (generateTypeDef manifest)) types
    val specs =
      [ valspec "backend" [] "string"
      , valspec "version" [] "string"
      , ""
      , typespec "ctx" []
      , exn_fut
      , type_cfg
      , valspec "default_cfg" [] "cfg"
      , valspec "ctx_new" ["cfg"] "ctx"
      , valspec "ctx_free" ["ctx"] "unit"
      , valspec "ctx_sync" ["ctx"] "unit"
      , ""
      ] @ type_specs @ ["", "structure Entry : sig"] @ map indent entry_specs
      @ ["end"]
    val defs =
      [ valdef "backend" (stringlit backend)
      , valdef "version" (stringlit version)
      , ""
      , typedef "ctx" [] (record_t [("cfg", pointer), ("ctx", pointer)])
      , exn_fut
      , type_cfg
      , typedef "futhark_context_config" [] pointer
      , typedef "futhark_context" [] pointer
      , "val default_cfg = " ^ def_cfg
      ] @ error_check
      @
      fundef "sync" ["ctx"]
        [apply "error_check"
           [ (fficall "futhark_context_sync" [("ctx", "futhark_context")] "int")
           , "ctx"
           ]]
      @
      fundef "ctx_new" ["{logging,debugging,profiling}"]
        [ "let"
        , "val c_cfg ="
        , fficall "futhark_context_config_new" [] "futhark_context_config"
        , "val () = "
          ^
          fficall "futhark_context_config_set_debugging"
            [ ("c_cfg", "futhark_context_config")
            , ("if debugging then 1 else 0", "int")
            ] "unit"
        , "val () ="
          ^
          fficall "futhark_context_config_set_logging"
            [ ("c_cfg", "futhark_context_config")
            , ("if logging then 1 else 0", "int")
            ] "unit"
        , "val () = "
          ^
          fficall "futhark_context_config_set_profiling"
            [ ("c_cfg", "futhark_context_config")
            , ("if profiling then 1 else 0", "int")
            ] "unit"
        , "val c_ctx ="
        , fficall "futhark_context_new" [("c_cfg", "futhark_context_config")]
            "futhark_context"
        , "in {cfg=c_cfg, ctx=c_ctx} end"
        ]
      @
      fundef "ctx_free" ["{cfg,ctx}"]
        [ "let"
        , "val () = "
          ^ fficall "futhark_context_free" [("ctx", "futhark_context")] "unit"
        , "val () = "
          ^
          fficall "futhark_context_config_free"
            [("cfg", "futhark_context_config")] "unit"
        , "in () end"
        ] @ fundef "ctx_sync" ["{cfg,ctx}"] [apply "sync" ["ctx"]] @ type_defs
      @ ["structure Entry = struct"] @ List.concat entry_defs @ ["end"]
  in
    ( unlines
        (array_signature @ [""] @ opaque_signature @ [""] @ record_signature
         @ [""] @ sigdef sig_name specs)
    , unlines (structdef struct_name (SOME sig_name) defs)
    )
  end

val signature_opt: string option ref = ref NONE
val structure_opt: string option ref = ref NONE
val output_opt: string option ref = ref NONE

fun options () : unit GetOpt.opt_descr list =
  [ { short = [#"h"]
    , long = ["help"]
    , arg = GetOpt.NO_ARG (fn () =>
        (print (usage ()); OS.Process.exit OS.Process.success))
    , desc = "Show help text."
    }
  , { short = []
    , long = ["signature-name"]
    , arg = GetOpt.REQ_ARG (fn s => signature_opt := SOME s, "NAME")
    , desc = "Use this signature name."
    }
  , { short = []
    , long = ["structure-name"]
    , arg = GetOpt.REQ_ARG (fn s => structure_opt := SOME s, "NAME")
    , desc = "USe this structure name."
    }
  , { short = [#"o"]
    , long = ["output-directory"]
    , arg = GetOpt.REQ_ARG (fn s => output_opt := SOME s, "DIR")
    , desc = "Put files here."
    }
  ]
and usage () =
  "Usage: smlfut [OPTIONS] MANIFEST.json\n" ^ GetOpt.usage (options ())

fun err s = TextIO.output (TextIO.stdErr, s)

fun main () =
  case GetOpt.getopt GetOpt.PERMUTE (options ()) (CommandLine.arguments ()) of
    (_, [json_file], []) =>
      let
        val base = OS.Path.base json_file
        val basefile = OS.Path.file base
        val m =
          manifestFromFile json_file
          handle IO.Io ({name = _, function = _, cause = OS.SysErr (_, SOME e)}) =>
            ( print (json_file ^ ": " ^ OS.errorMsg e ^ "\n")
            ; OS.Process.exit OS.Process.failure
            )
        val output_dir =
          case !output_opt of
            NONE => OS.Path.dir json_file
          | SOME s => s
        val sig_name =
          case !signature_opt of
            NONE => String.map Char.toUpper basefile
          | SOME s => s
        val struct_name =
          case !structure_opt of
            NONE => basefile
          | SOME s => s
        val (sig_s, struct_s) = generate sig_name struct_name m
      in
        checkValidName sig_name;
        checkValidName struct_name;
        writeFile (output_dir ^ "/" ^ basefile ^ ".sig") sig_s;
        writeFile (output_dir ^ "/" ^ basefile ^ ".sml") struct_s
      end
  | (_, _, errors) =>
      (List.app err errors; err (usage ()); OS.Process.exit OS.Process.failure)

val () = main ()
