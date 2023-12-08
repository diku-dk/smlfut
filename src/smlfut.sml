(* Generated signatures. *)

val sig_FUTHARK_POLY_ARRAY =
  [ "signature FUTHARK_POLY_ARRAY ="
  , "sig"
  , "  type array"
  , "  type ctx"
  , "  type shape"
  , "  type elem"
  , "  val new: ctx -> elem ArraySlice.slice -> shape -> array"
  , "  val free: array -> unit"
  , "  val shape: array -> shape"
  , "  val values: array -> elem Array.array"
  , "  val values_into: array -> elem ArraySlice.slice -> unit"
  , "end"
  ]

val sig_FUTHARK_MONO_ARRAY =
  [ "signature FUTHARK_MONO_ARRAY ="
  , "sig"
  , "  type array"
  , "  type ctx"
  , "  type shape"
  , "  structure Array : MONO_ARRAY"
  , "  structure Slice : MONO_ARRAY_SLICE"
  , "  val new: ctx -> Slice.slice -> shape -> array"
  , "  val free: array -> unit"
  , "  val shape: array -> shape"
  , "  val values: array -> Array.array"
  , "  val values_into: array -> Slice.slice -> unit"
  , "end"
  ]

val sig_FUTHARK_OPAQUE =
  [ "signature FUTHARK_OPAQUE ="
  , "sig"
  , "  type t"
  , "  type ctx"
  , "  val free : t -> unit"
  , "end"
  ]

val sig_FUTHARK_RECORD =
  [ "signature FUTHARK_RECORD ="
  , "sig"
  , "  include FUTHARK_OPAQUE"
  , "  type record"
  , "  val values : t -> record"
  , "  val new : ctx -> record -> t"
  , "end"
  ]

(* Actual logic. *)

fun gpuBackend "opencl" = true
  | gpuBackend "cuda" = true
  | gpuBackend "hip" = true
  | gpuBackend _ = false

fun constituent c =
  Char.isAlphaNum c orelse c = #"'" orelse c = #"_"

fun isValidName s =
  List.all constituent (explode s)

fun checkValidName s =
  if isValidName s then ()
  else raise Fail ("\"" ^ s ^ "\" is an invalid SML identifier.")

fun mkSum [] = "0"
  | mkSum [x] = x
  | mkSum (x :: xs) = x ^ "+" ^ mkSum xs


fun mkProd [] = "1"
  | mkProd [x] = x
  | mkProd (x :: xs) = x ^ "*" ^ mkProd xs

fun monoArrayModuleForType t =
  case t of
    "i8" => SOME "Int8Array"
  | "i16" => SOME "Int16Array"
  | "i32" => SOME "Int32Array"
  | "i64" => SOME "Int64Array"
  | "u8" => SOME "Word8Array"
  | "u16" => SOME "Word16Array"
  | "u32" => SOME "Word32Array"
  | "u64" => SOME "Word64Array"
  | "bool" => SOME "BoolArray"
  | "f16" => SOME "Real16Array"
  | "f32" => SOME "Real32Array"
  | "f64" => SOME "Real64Array"
  | _ => NONE


fun monoArrayModule (info: array_info) =
  case monoArrayModuleForType (#elemtype info) of
    SOME m => m
  | NONE =>
      raise Fail
        ("Cannot represent SML array with element type: " ^ #elemtype info)

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

fun futharkArrayStruct (info: array_info) =
  monoArrayModule info ^ Int.toString (#rank info)

fun shapeTypeOfRank d =
  (tuple_t o replicate d) "int"

fun smlArraySliceModule info = monoArrayModule info ^ "Slice"

fun valuesIntoFunction (array: array_info) =
  "futhark_values_into_" ^ #elemtype array ^ "_" ^ Int.toString (#rank array)
  ^ "d"

fun newSyncFunction (array: array_info) =
  "futhark_new_sync_" ^ #elemtype array ^ "_" ^ Int.toString (#rank array) ^ "d"


fun blankRef null manifest t =
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

signature TARGET =
sig
  val pointer: string
  val null: string
  val sig_FUTHARK_ARRAY: string list
  val futharkArrayStructSpec: array_info -> string list
  val futharkArrayStructDef: manifest -> array_info -> string list

  (* Invoke named C function with the provided (SML) arguments and return type *)
  val fficall: string -> (string * string) list -> string -> string
end

functor Smlfut(B: TARGET) =
struct

  open B

  fun cElemType "i8" = "int8_t"
    | cElemType "i16" = "int16_t"
    | cElemType "i32" = "int32_t"
    | cElemType "i64" = "int64_t"
    | cElemType "u8" = "uint8_t"
    | cElemType "u16" = "uint16_t"
    | cElemType "u32" = "uint32_t"
    | cElemType "u64" = "uint64_t"
    | cElemType "f16" = "uint16_t"
    | cElemType "f32" = "float"
    | cElemType "f64" = "double"
    | cElemType "bool" = "bool"
    | cElemType t =
        raise Fail ("cElemType: " ^ t)

  fun futharkArrayType (info: array_info) = futharkArrayStruct info ^ ".array"

  fun escapeName name =
    let
      fun escape c =
        if constituent c then c else #"_"
      val name' = String.map escape name
    in
      if name <> name' then "unrep_" ^ name' else name
    end

  fun futharkOpaqueStructInside name = escapeName name

  fun futharkOpaqueTypeInside name = futharkOpaqueStructInside name ^ ".t"

  fun futharkOpaqueStruct name = "Opaque." ^ futharkOpaqueStructInside name

  fun futharkOpaqueType name = futharkOpaqueStruct name ^ ".t"

  fun futharkTypeToSML name (FUTHARK_ARRAY info) = futharkArrayType info
    | futharkTypeToSML name (FUTHARK_OPAQUE info) = futharkOpaqueType name

  fun apiType t =
    case isPrimType t of
      SOME t' => t'
    | NONE => pointer

  fun mkOut null manifest t =
    case monoArrayModuleForType t of
      SOME m => apply (m ^ ".array") ["1", blankRef null manifest t]
    | NONE => apply "Word64Array.array" ["1", "0w0"]

  fun outType t =
    case monoArrayModuleForType t of
      SOME m => m ^ ".array"
    | NONE => "Word64Array.array"

  fun fetchOut v t =
    case monoArrayModuleForType t of
      SOME m => apply (m ^ ".sub") [v, "0"]
    | NONE =>
        fficall "smlfut_to_pointer"
          [(apply "Word64Array.sub" [v, "0"], "Word64.word")] pointer

  fun typeToSML manifest t =
    case lookupType manifest t of
      SOME t' => futharkTypeToSML t t'
    | NONE => primTypeToSML t

  fun typeToSMLInside manifest t =
    let
      fun futharkTypeToSML name (FUTHARK_ARRAY info) = futharkArrayType info
        | futharkTypeToSML name (FUTHARK_OPAQUE info) =
            futharkOpaqueTypeInside name
    in
      case lookupType manifest t of
        SOME t' => futharkTypeToSML t t'
      | NONE => primTypeToSML t
    end

  fun generateEntrySpec manifest (name, entry_point {cfun, inputs, outputs}) =
    valspec name ["ctx", tuple_t (map (typeToSML manifest o #type_) inputs)]
      (tuple_t (map (typeToSML manifest o #type_) outputs))

  val error_check =
    fundef "get_error" ["ctx"]
      (letbind
         [ ( "p"
           , fficall "futhark_context_get_error" [("ctx", "futhark_context")]
               pointer
           )
         , ("s", "strcpy p")
         , ("()", fficall "free" [("p", pointer)] "unit")
         ] ["s"])
    @
    fundef "error_check" ["(err,ctx)"]
      ["if err = 0 then () else raise Error (get_error(ctx))"]

  fun generateEntryDef manifest
    (name, ep as entry_point {cfun, inputs, outputs}) =
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
            ("out" ^ Int.toString i, mkOut null manifest type_)
            :: outDecs (i + 1) rest
      fun outArgs i [] = []
        | outArgs i (out :: rest) =
            ("out" ^ Int.toString i, outType (#type_ out))
            :: outArgs (i + 1) rest
      fun inpArgs i [] = []
        | inpArgs i (inp :: rest) =
            ("inp" ^ Int.toString i, apiType (#type_ inp))
            :: inpArgs (i + 1) rest
      fun outRes i [] = []
        | outRes i (out :: rest) =
            let
              val v = "out" ^ Int.toString i
              val fetch = fetchOut v (#type_ out)
            in
              (case lookupType manifest (#type_ out) of
                 SOME _ => tuple_e ["ctx", fetch]
               | _ => fetch) :: outRes (i + 1) rest
            end
    in
      fundef name (["{cfg,ctx}", tuple_e (inpParams 0 inputs)])
        (letbind
           (outDecs 0 outputs
            @
            [ ( "ret"
              , fficall cfun
                  ([("ctx", "futhark_context")] @ outArgs 0 outputs
                   @ inpArgs 0 inputs) "int"
              )
            , ("()", "error_check(ret, ctx)")
            ]) [tuple_e (outRes 0 outputs)])
    end

  fun origNameComment name =
    ["(* " ^ name ^ " *)"]

  fun generateTypeSpec manifest (name, FUTHARK_ARRAY info) =
        origNameComment name @ futharkArrayStructSpec info
    | generateTypeSpec manifest (name, FUTHARK_OPAQUE info) =
        case #record info of
          NONE =>
            origNameComment name
            @
            [ structspec (escapeName name) "FUTHARK_OPAQUE"
            , "where type ctx = ctx"
            ]
        | SOME record =>
            let
              fun fieldType (name, {project, type_}) =
                (name, typeToSMLInside manifest type_)
            in
              origNameComment name
              @
              [ structspec name "FUTHARK_RECORD"
              , "where type ctx = ctx"
              , "  and type record = "
                ^ record_t (map fieldType (#fields record))
              ]
            end

  fun generateTypeDef manifest
        (name, FUTHARK_ARRAY (info as {ctype, rank, elemtype, ops})) =
        futharkArrayStructDef manifest info
    | generateTypeDef manifest (name, FUTHARK_OPAQUE info) =
        let
          val more =
            case #record info of
              NONE => []
            | SOME record =>
                let
                  fun getField (name, {project, type_}) =
                    ( name
                    , "let val out = " ^ mkOut null manifest type_ ^ "in "
                      ^
                      apply "error_check"
                        [ fficall project
                            [ ("ctx", "futhark_context")
                            , ("out", outType type_)
                            , ("data", pointer)
                            ] "int"
                        , "ctx"
                        ] ^ "; "
                      ^
                      (case lookupType manifest type_ of
                         SOME _ => tuple_e ["ctx", fetchOut "out" type_]
                       | _ => fetchOut "out" type_) ^ " end"
                    )
                  fun fieldParam (name, {project, type_}) =
                    ( name
                    , case isPrimType type_ of
                        SOME _ => name
                      | NONE => tuple_e ["_", name]
                    )
                  fun fieldArg (name, {project, type_}) = (name, apiType type_)
                  fun fieldType (name, {project, type_}) =
                    (name, typeToSMLInside manifest type_)
                in
                  [typedef "record" [] (record_t
                     (map fieldType (#fields record)))]
                  @
                  fundef "values" ["(ctx,data)"]
                    [record_e (map getField (#fields record))]
                  @
                  fundef "new"
                    ["{cfg,ctx}", record_e (map fieldParam (#fields record))]
                    (letbind [("out", apply "Word64Array.array" ["1", "0w0"])]
                       [ apply "error_check"
                           [ (fficall (#new record)
                                ([ ("ctx", "futhark_context")
                                 , ("out", "Word64Array.array")
                                 ] @ map fieldArg (#fields record)) "int")
                           , "ctx"
                           ] ^ ";"
                       , tuple_e
                           [ "ctx"
                           , fficall "smlfut_to_pointer"
                               [( apply "Word64Array.sub" ["out", "0"]
                                , "Word64.word"
                                )] pointer
                           ]
                       ])
                end
        in
          structdef (escapeName name) NONE
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

  fun generateTypeCFuns (name, FUTHARK_OPAQUE _) = []
    | generateTypeCFuns (name, FUTHARK_ARRAY array) =
        let
          val name = valuesIntoFunction array
          val cet = cElemType (#elemtype array)
          val dim_names = List.tabulate (#rank array, fn i =>
            "dim" ^ Int.toString i)
          val dim_params = concat (intersperse ", "
            (map (fn s => "int64_t " ^ s) dim_names))
        in
          [ "int " ^ #values (#ops array) ^ "(void *ctx, void* arr, " ^ cet
            ^ " *data);"
          , "int " ^ name ^ "(void *ctx, void* arr, " ^ cet
            ^ " *data, int64_t offset);"
          , "int " ^ name ^ "(void *ctx, void* arr, " ^ cet
            ^ " *data, int64_t offset) {"
          , "  int ret = " ^ #values (#ops array) ^ "(ctx, arr, data+offset);"
          , "  if (ret == 0) { return futhark_context_sync(ctx); }"
          , "  else { return ret; }"
          , "}"
          , "void* " ^ #new (#ops array) ^ "(void *ctx, " ^ cet ^ " *data, "
            ^ dim_params ^ ");"
          , "void* " ^ #free (#ops array) ^ "(void *ctx, void* arr);"
          , "void* " ^ newSyncFunction array ^ "(void *ctx, " ^ cet
            ^ " *data, int64_t i," ^ dim_params ^ ");"
          , "void* " ^ newSyncFunction array ^ "(void *ctx, " ^ cet
            ^ " *data, int64_t i," ^ dim_params ^ ") {"
          , "  void* arr = " ^ #new (#ops array) ^ "(ctx, data+i, "
            ^ concat (intersperse ", " dim_names) ^ ");"
          , "  if (arr == NULL) { return NULL; }"
          , "  if (futhark_context_sync(ctx) != 0) { " ^ #free (#ops array)
            ^ "(ctx, arr); return NULL; }"
          , "  return arr;"
          , "}"
          ]
        end

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
      fun isArray (_, FUTHARK_ARRAY _) = true
        | isArray _ = false
    in
      List.partition isArray (order [] types)
    end

  fun cfgSetterSpec (name, type_) =
    valspec name [type_, "cfg"] "cfg"

  fun cfgSetterDef cfg_fields (name, type_) =
    fundef name
      ["v", record_e (ListPair.zip (map #1 cfg_fields, map #1 cfg_fields))]
      [record_e (map (fn (x, _) => (x, if x = name then "v" else x)) cfg_fields)]

  val header = ["(* Generated by smlfut " ^ version ^ " *)", ""]

  fun generate sig_name struct_name
    (manifest as MANIFEST {backend, version, entry_points, types}) =
    let
      val cfg_fields =
        [ ("logging", "bool")
        , ("debugging", "bool")
        , ("profiling", "bool")
        , ("cache", "string option")
        , ("tuning", "(string * int) list")
        ] @ (if gpuBackend backend then [("device", "string option")] else [])
      val cfg_type = typedef "cfg" [] (record_t cfg_fields)
      val cfg_default = record_e
        ([ ("logging", "false")
         , ("debugging", "false")
         , ("profiling", "false")
         , ("cache", "NONE")
         , ("tuning", "[]")
         ] @ (if gpuBackend backend then [("device", "NONE")] else []))
      val exn_fut = "exception Error of string"
      val entry_specs = map (generateEntrySpec manifest) entry_points
      val entry_defs = List.concat
        (map (generateEntryDef manifest) entry_points)
      val (array_types, opaque_types) = orderTypes types
      val array_type_specs =
        (List.concat o intersperse [""] o map (generateTypeSpec manifest))
          array_types
      val array_type_defs =
        (List.concat o intersperse [""] o map (generateTypeDef manifest))
          array_types
      val opaque_type_specs =
        (List.concat o intersperse [""] o map (generateTypeSpec manifest))
          opaque_types
      val opaque_type_defs =
        (List.concat o intersperse [""] o map (generateTypeDef manifest))
          opaque_types
      val cfuns = (List.concat o intersperse [""] o map generateTypeCFuns) types
      val specs =
        [ valspec "backend" [] "string"
        , valspec "version" [] "string"
        , ""
        , typespec "ctx" []
        , exn_fut
        , cfg_type
        , ""
        , "structure Config : sig"
        ]
        @
        map indent (valspec "default" [] "cfg" :: map cfgSetterSpec cfg_fields)
        @
        [ "end"
        , ""
        , "structure Context : sig"
        , indent (valspec "new" ["cfg"] "ctx")
        , indent (valspec "free" ["ctx"] "unit")
        , indent (valspec "sync" ["ctx"] "unit")
        , indent (valspec "report" ["ctx"] "string")
        , indent (valspec "pauseProfiling" ["ctx"] "unit")
        , indent (valspec "unpauseProfiling" ["ctx"] "unit")
        , indent (valspec "clearCaches" ["ctx"] "unit")
        , "end"
        , ""
        ] @ array_type_specs @ ["", "structure Opaque : sig"]
        @ map indent opaque_type_specs @ ["end", "", "structure Entry : sig"]
        @ map indent entry_specs @ ["end"]
      val defs =
        [ ""
        , valdef "backend" (stringlit backend)
        , valdef "version" (stringlit version)
        , ""
        , typedef "ctx" [] (record_t [("cfg", pointer), ("ctx", pointer)])
        , exn_fut
        , cfg_type
        , typedef "futhark_context_config" [] pointer
        , typedef "futhark_context" [] pointer
        ] @ [valdef "NULL" (fficall "mknull" [] pointer)]
        @
        fundef "strcpy" ["p"]
          (letbind
             [ ("n", fficall "smlfut_strlen" [("p", pointer)] "Int64.int")
             , ("s", "CharVector.tabulate (Int64.toInt n, fn i => chr 0)")
             , ( "_"
               , fficall "smlfut_memcpy"
                   [("s", "string"), ("p", pointer), ("n", "Int64.int")] pointer
               )
             ] ["s"]) @ error_check
        @
        structdef "Config" NONE
          (valdef "default" cfg_default
           :: List.concat (map (cfgSetterDef cfg_fields) cfg_fields))
        @
        structdef "Context" NONE
          (fundef "new" ["(cfg : cfg)"]
             (letbind
                ([ ( "c_cfg"
                   , fficall "futhark_context_config_new" []
                       "futhark_context_config"
                   )
                 , ( "setTuningParam"
                   , ("fn (v,x) => "
                      ^
                      unwords
                        (letbind
                           [ ( "v'"
                             , fficall "mk_cstring"
                                 [ ("v", "string")
                                 , ("Int64.fromInt (size v)", "Int64.int")
                                 ] pointer
                             )
                           , ( "e"
                             , fficall "futhark_context_config_set_tuning_param"
                                 [ ("c_cfg", "futhark_context_config")
                                 , ("v'", pointer)
                                 , ("Int64.fromInt x", "Int64.int")
                                 ] "int"
                             )
                           , ("()", fficall "free" [("v'", pointer)] "unit")
                           ]
                           ["if " ^ "e <> 0 then"
                            ^
                            parens
                              (fficall "futhark_context_config_free"
                                 [("c_cfg", "futhark_context_config")] "unit"
                               ^
                               "; raise Error (\"Unknown tuning parameter: \" ^ v")
                            ^ ") else ()"]))
                   )
                 , ( "()"
                   , fficall "futhark_context_config_set_debugging"
                       [ ("c_cfg", "futhark_context_config")
                       , ("if #debugging cfg then 1 else 0", "int")
                       ] "unit"
                   )
                 , ( "()"
                   , fficall "futhark_context_config_set_logging"
                       [ ("c_cfg", "futhark_context_config")
                       , ("if #logging cfg then 1 else 0", "int")
                       ] "unit"
                   )
                 , ( "()"
                   , fficall "futhark_context_config_set_profiling"
                       [ ("c_cfg", "futhark_context_config")
                       , ("if #profiling cfg then 1 else 0", "int")
                       ] "unit"
                   )
                 , ( "()"
                   , unwords
                       (["case #cache cfg of", "NONE => ()", "| SOME f =>"]
                        @
                        letbind
                          [ ( "f'"
                            , fficall "mk_cstring"
                                [ ("f", "string")
                                , ("Int64.fromInt (size f)", "Int64.int")
                                ] pointer
                            )
                          , ( "()"
                            , fficall "futhark_context_config_set_cache_file"
                                [ ("c_cfg", "futhark_context_config")
                                , ("f", "string")
                                ] "unit"
                            )
                          ] [fficall "free" [("f'", pointer)] "unit"])
                   )
                 , ("()", "List.app setTuningParam (#tuning cfg)")
                 ]
                 @
                 (if gpuBackend backend then
                    [( "()"
                     , "case #device cfg of SOME d => "
                       ^
                       fficall "futhark_context_config_set_device"
                         [("c_cfg", "futhark_context_config"), ("d", "string")]
                         "unit" ^ " | NONE => ()"
                     )]
                  else
                    [])
                 @
                 [( "c_ctx"
                  , fficall "futhark_context_new"
                      [("c_cfg", "futhark_context_config")] "futhark_context"
                  )]) ["{cfg=c_cfg, ctx=c_ctx}"])
           @
           fundef "free" ["{cfg,ctx}"]
             [ "let"
             , "val () = "
               ^
               fficall "futhark_context_free" [("ctx", "futhark_context")]
                 "unit"
             , "val () = "
               ^
               fficall "futhark_context_config_free"
                 [("cfg", "futhark_context_config")] "unit"
             , "in () end"
             ]
           @
           fundef "sync" ["{cfg,ctx}"]
             [apply "error_check"
                [ (fficall "futhark_context_sync" [("ctx", "futhark_context")]
                     "int")
                , "ctx"
                ]]
           @
           fundef "report" ["{cfg,ctx}"]
             (letbind
                [ ( "p"
                  , fficall "futhark_context_report"
                      [("ctx", "futhark_context")] pointer
                  )
                , ("s", "strcpy p")
                , ("()", fficall "free" [("p", pointer)] "unit")
                ] ["s"])
           @
           fundef "pauseProfiling" ["{cfg,ctx}"]
             [fficall "futhark_context_pause_profiling"
                [("ctx", "futhark_context")] "unit"]
           @
           fundef "unpauseProfiling" ["{cfg,ctx}"]
             [fficall "futhark_context_unpause_profiling"
                [("ctx", "futhark_context")] "unit"]
           @
           fundef "clearCaches" ["{cfg,ctx}"]
             [fficall "futhark_context_clear_caches"
                [("ctx", "futhark_context")] "unit"]) @ array_type_defs
        @ ["structure Opaque = struct"] @ map indent opaque_type_defs @ ["end"]
        @ ["structure Entry = struct"] @ map indent entry_defs @ ["end"]
    in
      ( unlines
          (header @ sig_FUTHARK_ARRAY @ [""] @ sig_FUTHARK_OPAQUE @ [""]
           @ sig_FUTHARK_RECORD @ [""] @ sigdef sig_name specs)
      , unlines (header @ structdef struct_name (SOME sig_name) defs)
      , unlines
          ([ "#include <stdint.h>"
           , "#include <stddef.h>"
           , "#include <stdbool.h>"
           , "#include <stdlib.h>"
           , "#include <string.h>"
           , ""
           , "int futhark_context_sync(void*);"
           , ""
           , "uintptr_t mknull(void) { return (uintptr_t)NULL; }"
           , ""
           , "void* mk_cstring (const char* s, int64_t n) {"
           , "  char *out = malloc((size_t)n + 1);"
           , "  out[n] = 0;"
           , "  strncpy(out, s, (size_t)n);"
           , "  return out;"
           , "}"
           , ""
           , "void* smlfut_memcpy(void* d, const void* s, int64_t n) {"
           , "  return memcpy(d, s, n);"
           , "}"
           , ""
           , "int64_t smlfut_strlen(const char *s) {"
           , "  return strlen(s);"
           , "}"
           , ""
           , "void* smlfut_to_pointer(uint64_t x) {"
           , "  return (void*)(uintptr_t)x;"
           , "}"
           ] @ cfuns)
      )
    end
end

structure MonoDefs =
struct
  val sig_FUTHARK_ARRAY = sig_FUTHARK_MONO_ARRAY
  fun smlArrayType (info: array_info) =
    primTypeToSML (#elemtype info) ^ " Array.array"
  fun smlArrayType info = monoArrayModule info ^ ".array"
  fun futharkArrayStructSpec (info: array_info) =
    [ structspec (futharkArrayStruct info) "FUTHARK_MONO_ARRAY"
    , "where type ctx = ctx"
    , "  and type shape = " ^ shapeTypeOfRank (#rank info)
    , "  and type Array.array = " ^ monoArrayModule info ^ ".array"
    , "  and type Array.elem = " ^ monoArrayModule info ^ ".elem"
    , "  and type Slice.slice = " ^ smlArraySliceModule info ^ ".slice"
    , "  and type Slice.elem = " ^ smlArraySliceModule info ^ ".elem"
    ]
end

structure PolyDefs =
struct
  val sig_FUTHARK_ARRAY = sig_FUTHARK_POLY_ARRAY
  fun futharkArrayStructSpec (info: array_info) =
    [ structspec (futharkArrayStruct info) "FUTHARK_POLY_ARRAY"
    , "where type ctx = ctx"
    , "  and type shape = " ^ shapeTypeOfRank (#rank info)
    , "  and type elem = " ^ primTypeToSML (#elemtype info)
    ]
  fun smlArrayType (info: array_info) =
    primTypeToSML (#elemtype info) ^ " Array.array"
end

local

  fun mkSize fficall pointer (info: array_info) v =
    letbind
      [ ( "shape_c"
        , fficall (#shape (#ops info))
            [("ctx", "futhark_context"), (v, pointer)] pointer
        )
      , ("r", Int.toString (#rank info))
      , ("shape_ml", apply "Int64Array.array" ["r", "0"])
      , ( "_"
        , fficall "smlfut_memcpy"
            [ ("shape_ml", "Int64Array.array")
            , ("shape_c", pointer)
            , (apply "Int64.fromInt" ["r*8"], "Int64.int")
            ] pointer
        )
      ]
      [(mkProd (List.tabulate (#rank info, fn i =>
          apply "Int64.toInt"
            [apply "Int64Array.sub" ["shape_ml", Int.toString i]])))]


  fun mkShape fficall pointer (info: array_info) v =
    letbind
      [ ( "shape_c"
        , fficall (#shape (#ops info))
            [("ctx", "futhark_context"), (v, pointer)] pointer
        )
      , ("r", Int.toString (#rank info))
      , ("shape_ml", apply "Int64Array.array" [Int.toString (#rank info), "0"])
      , ( "_"
        , fficall "smlfut_memcpy"
            [ ("shape_ml", "Int64Array.array")
            , ("shape_c", pointer)
            , (apply "Int64.fromInt" ["r*8"], "Int64.int")
            ] pointer
        )
      ]
      [tuple_e (List.tabulate (#rank info, fn i =>
         apply "Int64.toInt"
           [apply "Int64Array.sub" ["shape_ml", Int.toString i]]))]
in
  fun futharkArrayStructDef fficall pointer null defs (data_t: string) manifest
    (info as {ctype, rank, elemtype, ops}) =
    let
      val shape =
        if rank = 1 then
          ["shape"]
        else
          List.tabulate (rank, fn i =>
            parens ("#" ^ Int.toString (i + 1) ^ " shape"))
      val shape_args =
        map (fn x => (apply "Int64.fromInt" [x], "Int64.int")) shape
    in
      structdef (futharkArrayStruct info) NONE
        ([ typedef "array" [] (tuple_t ["futhark_context", pointer])
         , typedef "ctx" [] "ctx"
         , typedef "shape" [] (shapeTypeOfRank rank)
         ] @ defs
         @
         fundef "new"
           ["{ctx,cfg}", "slice", parens ("shape: " ^ shapeTypeOfRank rank)]
           (letbind
              [ ("(arr,i,n)", "Slice.base slice")
              , ("()", "if " ^ mkProd shape ^ " <> n then raise Size else ()")
              , ( "arr"
                , fficall (newSyncFunction info)
                    ([ ("ctx", "futhark_context")
                     , ("arr", data_t)
                     , ("Int64.fromInt i", "Int64.int")
                     ] @ shape_args) pointer
                )
              ]
              [ "if arr = " ^ null
              , "then raise Error (get_error ctx)"
              , "else (ctx, arr)"
              ])
         @
         fundef "free" ["(ctx,data)"]
           [apply "error_check"
              [ (fficall (#free ops)
                   ([("ctx", "futhark_context"), ("data", pointer)]) "int")
              , "ctx"
              ]]
         @ fundef "shape" ["(ctx,data)"] (mkShape fficall pointer info "data")
         @
         fundef "values_into" ["(ctx, data)", "slice"]
           (letbind
              [ ("(arr, i, n)", "Slice.base slice")
              , ("m", unlines (mkSize fficall pointer info "data"))
              , ("()", "if n <> m then raise Size else ()")
              , ( "()"
                , apply "error_check"
                    [ fficall (valuesIntoFunction info)
                        [ ("ctx", "futhark_context")
                        , ("data", pointer)
                        , ("arr", data_t)
                        , (apply "Int64.fromInt" ["i"], "Int64.int")
                        ] "int"
                    , "ctx"
                    ]
                )
              ] ["()"])
         @
         fundef "values" ["(ctx, data)"]
           (letbind
              [ ("n", unlines (mkSize fficall pointer info "data"))
              , ( "out"
                , apply ("Array.array") ["n", blankRef null manifest elemtype]
                )
              , ("()", "values_into (ctx, data) (Slice.full out)")
              ] ["out"]))
    end
end

local
  (* Extracting the Futhark error string is somewhat tricky, for two reasons:
  
  1) We have to convert it to an SML string.
  
  2) We are responsible for freeing the C string.
  
  Our solution is to allocate an SML string, copy the C string into it,
  then free the C string.
   *)
  structure MLtonDefs =
  struct
    val pointer = "MLton.Pointer.t"
    val null = "MLton.Pointer.null"

    fun fficall cfun args ret =
      let
        val (arg_es, arg_ts) = ListPair.unzip args
      in
        apply
          (parens
             ("_import \"" ^ cfun ^ "\" public : " ^ tuple_t arg_ts ^ " -> "
              ^ ret ^ ";")) arg_es
      end
  end

  val fficall = MLtonDefs.fficall
in
  structure MLtonMono =
    Smlfut
      (struct
         open MonoDefs
         open MLtonDefs
         val futharkArrayStructDef = fn manifest =>
           fn info =>
             futharkArrayStructDef fficall pointer null
               [ "structure Array = " ^ monoArrayModule info
               , "structure Slice = " ^ smlArraySliceModule info
               ] (smlArrayType info) manifest info
       end)

  structure MLtonPoly =
    Smlfut
      (struct
         open PolyDefs
         open MLtonDefs
         val futharkArrayStructDef = fn manifest =>
           fn info =>
             futharkArrayStructDef fficall pointer null
               [ "structure Array = Array"
               , "structure Slice = ArraySlice"
               , "type elem = " ^ primTypeToSML (#elemtype info)
               ] (smlArrayType info) manifest info
       end)
end

structure MLKit =
  Smlfut
    (struct
       open MonoDefs
       val pointer = "foreignptr"
       val null = "NULL"
       (* Note that we always do auto-conversion here. *)
       fun fficall cfun args ret =
         ("prim "
          ^
          parens
            ("\"@" ^ cfun ^ "\", "
             ^ tuple_e (map (fn (x, t) => parens (x ^ " : " ^ t)) args)) ^ " : "
          ^ ret)

       val util_defs = []
       val futharkArrayStructDef = fn manifest =>
         fn info =>
           futharkArrayStructDef fficall pointer null
             [ "structure Array = " ^ monoArrayModule info
             , "structure Slice = " ^ smlArraySliceModule info
             ] (smlArrayType info) manifest info

     end)

datatype target = MLTON_MONO | MLTON_POLY | MLKIT

val signature_opt: string option ref = ref NONE
val structure_opt: string option ref = ref NONE
val output_opt: string option ref = ref NONE
val target = ref MLTON_POLY

fun showVersion () =
  ( print ("smlfut " ^ version ^ "\n")
  ; print ("Developed at the University of Copenhagen\n")
  ; print ("Released under the terms of the GPLv3 or later.\n")
  ; OS.Process.exit OS.Process.success
  )

fun options () : unit GetOpt.opt_descr list =
  [ { short = [#"h"]
    , long = ["help"]
    , arg = GetOpt.NO_ARG (fn () =>
        (print (usage ()); OS.Process.exit OS.Process.success))
    , desc = "Show help text."
    }
  , { short = [#"V"]
    , long = ["version"]
    , arg = GetOpt.NO_ARG showVersion
    , desc = "Show version information"
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
  , { short = []
    , long = ["target"]
    , arg = GetOpt.REQ_ARG
        ( fn s =>
            target
            :=
            (case s of
               "mlton-poly" => MLTON_POLY
             | "mlton-mono" => MLTON_MONO
             | "mlkit" => MLKIT
             | _ =>
                 ( print ("Unknown target: " ^ s)
                 ; OS.Process.exit OS.Process.failure
                 ))
        , "TARGET"
        )
    , desc = "Target MLton with polymorphic arrays"
    }
  , { short = []
    , long = ["poly-arrays"]
    , arg = GetOpt.NO_ARG (fn () => target := MLTON_POLY)
    , desc = "Equivalent to --target=mlton-poly"
    }
  , { short = []
    , long = ["mono-arrays"]
    , arg = GetOpt.NO_ARG (fn () => target := MLTON_MONO)
    , desc = "Equivalent to --target=mlton-mono"
    }
  ]
and usage () =
  "Usage: smlfut [OPTIONS] MANIFEST.json\n" ^ GetOpt.usage (options ())

fun err s = TextIO.output (TextIO.stdErr, s)

fun localIfEmpty "" = "."
  | localIfEmpty s = s

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
            NONE => localIfEmpty (OS.Path.dir json_file)
          | SOME s => s
        val sig_name =
          case !signature_opt of
            NONE => String.map Char.toUpper basefile
          | SOME s => s
        val struct_name =
          case !structure_opt of
            NONE => basefile
          | SOME s => s
        val generate =
          case !target of
            MLTON_MONO => MLtonMono.generate
          | MLTON_POLY => MLtonPoly.generate
          | MLKIT => MLKit.generate
        val (sig_s, struct_s, c_s) = generate sig_name struct_name m
      in
        checkValidName sig_name;
        checkValidName struct_name;
        writeFile (output_dir ^ "/" ^ basefile ^ ".sig") sig_s;
        writeFile (output_dir ^ "/" ^ basefile ^ ".sml") struct_s;
        writeFile (output_dir ^ "/" ^ basefile ^ ".smlfut.c") c_s
      end
  | (_, _, errors) =>
      (List.app err errors; err (usage ()); OS.Process.exit OS.Process.failure)

val () = main ()
