(* Invoke named C function with the provided (SML) arguments and return type *)
fun fficall cfun args ret =
  let
    val (arg_es, arg_ts) = ListPair.unzip args
  in
    apply
      (parens
         ("_import \"" ^ cfun ^ "\" public : " ^ tuplify_t arg_ts ^ " -> " ^ ret
          ^ ";")) arg_es
  end

val null = "MLton.Pointer.null"

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
  "array_" ^ Int.toString (#rank info) ^ "d" ^ "_" ^ #elemtype info

fun futharkArrayType (info: array_info) = futharkArrayStruct info ^ ".array"

fun futharkOpaqueStruct name = "opaque_" ^ name

fun futharkOpaqueType name = futharkOpaqueStruct name ^ ".t"

fun futharkTypeToSML name (FUTHARK_ARRAY info) = futharkArrayType info
  | futharkTypeToSML name (FUTHARK_OPAQUE info) = futharkOpaqueType name

fun isPrimType "i8" = SOME "Int8.int"
  | isPrimType "i16" = SOME "Int16.int"
  | isPrimType "i32" = SOME "Int32.int"
  | isPrimType "i64" = SOME "Int64.int"
  | isPrimType "u8" = SOME "Word8.word"
  | isPrimType "u16" = SOME "Word16.word"
  | isPrimType "u32" = SOME "Word32.word"
  | isPrimType "u64" = SOME "Word64.word"
  | isPrimType "bool" = SOME "bool"
  | isPrimType _ = NONE

fun primTypeToSML t =
  case isPrimType t of
    SOME t' => t'
  | NONE => raise Fail ("Cannot map type to SML: " ^ t)

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
  valspec ("entry_" ^ name) ("ctx" :: map (typeToSML manifest o #type_) inputs)
    (tuplify_t (map (typeToSML manifest o #type_) outputs))

fun mkSum [] = "0"
  | mkSum [x] = x
  | mkSum (x :: xs) = x ^ "+" ^ mkSum xs


fun mkProd [] = "1"
  | mkProd [x] = x
  | mkProd (x :: xs) = x ^ "*" ^ mkProd xs

fun mkSize (info: array_info) v =
  "let val shape_c = "
  ^
  fficall (#shape (#ops info)) [("ctx", "futhark_context"), (v, "pointer")]
    "pointer" ^ " in "
  ^
  mkProd (List.tabulate (#rank info, fn i =>
    apply "Int64.toInt"
      [apply "MLton.Pointer.getInt64" ["shape_c", Int.toString i]])) ^ " end"

fun mkShape (info: array_info) v =
  "let val shape_c = "
  ^
  fficall (#shape (#ops info)) [("ctx", "futhark_context"), (v, "pointer")]
    "pointer" ^ " in "
  ^
  tuplify_e (List.tabulate (#rank info, fn i =>
    apply "Int64.toInt"
      [apply "MLton.Pointer.getInt64" ["shape_c", Int.toString i]])) ^ " end"

(* Extracting the Futhark error string is somewhat tricky, for two reasons:

1) We have to convert it to an SML string.

2) We are responsible for freeing the C string.

Our solution is to allocate an SML string, copy the C string into it,
then free the C string.
 *)
val error_check: string = fundef "error_check" ["(err,ctx)"] (unlines
  [ "if err = 0 then () else"
  , "let val p = "
    ^ fficall "futhark_context_get_error" [("ctx", "futhark_context")] "pointer"
  , "val n = " ^ fficall "strlen" [("p", "pointer")] "Int64.int"
  , "val s = " ^ apply "CharVector.tabulate" ["Int64.toInt n", "fn _ => #\" \""]
  , "in"
  , fficall "strcpy" [("s", "string"), ("p", "pointer")] "unit" ^ ";"
  , fficall "free" [("p", "pointer")] "unit" ^ ";"
  , "raise error s"
  , "end"
  ])

fun generateEntryDef manifest (name, ep as entry_point {cfun, inputs, outputs}) =
  let
    fun apiType t =
      case isPrimType t of
        SOME t' => t'
      | NONE => "pointer"
    fun inpParams i [] = []
      | inpParams i ({name = _, type_, unique = _} :: rest) =
          let
            val v = "inp" ^ Int.toString i
          in
            (case lookupType manifest type_ of
               SOME _ => "(_, " ^ v ^ ")"
             | _ => v) :: inpParams (i + 1) rest
          end
    fun outDecs i [] = ""
      | outDecs i ({type_, unique = _} :: rest) =
          "val out" ^ Int.toString i ^ " = ref (" ^ blankRef manifest type_
          ^ ")" ^ "\n" ^ outDecs (i + 1) rest
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
               SOME _ => tuplify_e ["ctx", "!" ^ v]
             | _ => "!" ^ v) :: outRes (i + 1) rest
          end
  in
    fundef ("entry_" ^ name) (["{cfg,ctx}"] @ (inpParams 0 inputs))
      ("let\n" ^ outDecs 0 outputs ^ "val ret = "
       ^
       fficall cfun
         ([("ctx", "futhark_context")] @ outArgs 0 outputs @ inpArgs 0 inputs)
         "Int32.int" ^ "\nin error_check(ret,ctx); "
       ^ tuplify_e (outRes 0 outputs) ^ " end")
  end

fun shapeTypeOfRank d =
  (tuplify_t o replicate d) "int"

fun generateTypeSpec manifest (name, FUTHARK_ARRAY info) =
      unlines
        [ "structure " ^ futharkArrayStruct info ^ " : FUTHARK_ARRAY"
        , "where type ctx = ctx"
        , "  and type shape = " ^ shapeTypeOfRank (#rank info)
        , "  and type native.array = " ^ smlArrayModule info ^ ".array"
        , "  and type native.elem = " ^ smlArrayModule info ^ ".elem"
        ]
  | generateTypeSpec manifest (name, FUTHARK_OPAQUE info) =
      case #record info of
        NONE =>
          unlines
            [ "structure " ^ futharkOpaqueStruct name ^ " : FUTHARK_OPAQUE"
            , "where type ctx = ctx"
            ]
      | SOME record =>
          let
            fun fieldType (name, {project, type_}) =
              (name, typeToSML manifest type_)
          in
            unlines
              [ "structure " ^ futharkOpaqueStruct name ^ " : FUTHARK_RECORD"
              , "where type ctx = ctx"
              , "  and type record = "
                ^ record_t (map fieldType (#fields record))
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
        unlines
          [ "structure " ^ futharkArrayStruct info ^ " = struct"
          , typedef "array" [] (tuplify_t ["futhark_context", "pointer"])
          , typedef "ctx" [] "ctx"
          , typedef "shape" [] (shapeTypeOfRank rank)
          , "structure native = " ^ smlArrayModule info
          , fundef "new"
              [ "{ctx,cfg}"
              , parens ("data: " ^ data_t)
              , parens ("shape: " ^ shapeTypeOfRank rank)
              ]
              (tuplify_e
                 [ "ctx"
                 , fficall (#new ops)
                     ([("ctx", "futhark_context"), ("data", data_t)]
                      @ shape_args) "pointer"
                 ])
          , fundef "free" ["(ctx,data)"] (apply "error_check"
              [ (fficall (#free ops)
                   ([("ctx", "futhark_context"), ("data", "pointer")]) "int")
              , "ctx"
              ])
          , fundef "shape" ["(ctx,data)"] (mkShape info "data")
          , fundef "values" ["(ctx, data)"] (unlines
              [ "let"
              , "val n = " ^ mkSize info "data"
              , "val out = "
                ^
                apply (smlArrayModule info ^ ".array")
                  ["n", blankRef manifest elemtype]
              , "val err = "
                ^
                fficall (#values ops)
                  [ ("ctx", "futhark_context")
                  , ("data", "pointer")
                  , ("out", data_t)
                  ] "Int32.int"
              , "in out end"
              ])
          , "end"
          ]
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
                          , ("out", typeToSML manifest type_ ^ " ref")
                          , ("data", "pointer")
                          ] "int"
                      , "ctx"
                      ] ^ "; "
                    ^
                    (case lookupType manifest type_ of
                       SOME _ => tuplify_e ["ctx", "!out"]
                     | _ => "!out") ^ " end"
                  )
                fun fieldParam (name, _) = (name, name)
                fun fieldArg (name, {project, type_}) =
                  (name, typeToSML manifest type_)
                fun fieldType (name, {project, type_}) =
                  (name, typeToSML manifest type_)
              in
                [ typedef "record" [] (record_t
                    (map fieldType (#fields record)))
                , fundef "to_record" ["(ctx,data)"] (record_e
                    (map getField (#fields record)))
                , fundef "from_record"
                    ["{cfg,ctx}", record_e (map fieldParam (#fields record))]
                    ("let val out = ref " ^ null ^ " in "
                     ^
                     apply "error_check"
                       [ (fficall (#new record)
                            ([ ("ctx", "futhark_context")
                             , ("out", "pointer ref")
                             ] @ map fieldArg (#fields record)) "int")
                       , "ctx"
                       ] ^ ";(ctx,!out) end")
                ]
              end
      in
        unlines
          ([ "structure " ^ futharkOpaqueStruct name ^ " = struct"
           , typedef "ctx" [] "ctx"
           , typedef "t" [] (tuplify_t ["futhark_context", "pointer"])
           , fundef "free" ["(ctx,data)"] (apply "error_check"
               [ (fficall (#free (#ops info))
                    ([("ctx", "futhark_context"), ("data", "pointer")]) "int")
               , "ctx"
               ])
           ] @ more @ ["end"])
      end

val array_signature =
  [ "signature FUTHARK_ARRAY ="
  , "sig"
  , "type array"
  , "type ctx"
  , "type shape"
  , "structure native : MONO_ARRAY"
  , "val new: ctx -> native.array -> shape -> array"
  , "val free: array -> unit"
  , "val shape: array -> shape"
  , "val values: array -> native.array"
  , "end"
  ]


val opaque_signature =
  [ "signature FUTHARK_OPAQUE ="
  , "sig"
  , "type t"
  , "type ctx"
  , "val free : t -> unit"
  , "end"
  ]

val record_signature =
  [ "signature FUTHARK_RECORD ="
  , "sig"
  , "include FUTHARK_OPAQUE"
  , "type record"
  , "val to_record : t -> record"
  , "val from_record : ctx -> record -> t"
  , "end"
  ]

fun generate sig_name struct_name
  (manifest as MANIFEST {backend, entry_points, types}) =
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
    val type_specs = map (generateTypeSpec manifest) types
    val type_defs = map (generateTypeDef manifest) types
    val specs =
      [ typespec "ctx" []
      , exn_fut
      , type_cfg
      , valspec "default_cfg" [] "cfg"
      , valspec "ctx_new" ["cfg"] "ctx"
      , valspec "ctx_free" ["ctx"] "unit"
      , ""
      ] @ type_specs @ entry_specs
    val defs =
      [ typedef "pointer" [] "MLton.Pointer.t"
      , typedef "ctx" [] "{cfg: pointer, ctx: pointer}"
      , exn_fut
      , type_cfg
      , typedef "futhark_context_config" [] "pointer"
      , typedef "futhark_context" [] "pointer"
      , "val default_cfg = " ^ def_cfg
      , ""
      , error_check
      , fundef "ctx_new" ["{logging,debugging,profiling}"] (unlines
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
          ])
      , fundef "ctx_free" ["{cfg,ctx}"] (unlines
          [ "let"
          , "val () = "
            ^ fficall "futhark_context_free" [("ctx", "futhark_context")] "unit"
          , "val () = "
            ^
            fficall "futhark_context_config_free"
              [("cfg", "futhark_context_config")] "unit"
          , "in () end"
          ])
      ] @ type_defs @ entry_defs
  in
    ( unlines
        (array_signature @ [""] @ opaque_signature @ [""] @ record_signature
         @ ["", "signature " ^ sig_name ^ " = sig"] @ specs @ ["end"])
    , unlines
        (["structure " ^ struct_name ^ " :> " ^ sig_name ^ " = struct"] @ defs
         @ ["end"])
    )
  end

fun main () =
  case CommandLine.arguments () of
    [json_file] =>
      let
        val base = Path.base json_file
        val m = manifestFromFile json_file
        val (sig_name, struct_name) =
          (String.map Char.toUpper (Path.file base), (Path.file base))
        val (sig_s, struct_s) = generate sig_name struct_name m
      in
        writeFile (base ^ ".sig") sig_s;
        writeFile (base ^ ".sml") struct_s
      end
  | _ =>
      ( TextIO.output (TextIO.stdErr, "Need a Futhark manifest file.\n")
      ; Process.exit Process.failure
      )

val () = main ()
