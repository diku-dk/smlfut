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

fun smlArrayModule (info: array_info) =
  case #elemtype info of
    "i32" => "Int32Array"
  | _ =>
      raise Fail
        ("Cannot represent SML array with element type: " ^ #elemtype info)

fun smlArrayType info = smlArrayModule info ^ ".array"

fun futharkArrayType (info: array_info) =
  "array_" ^ #elemtype info ^ "_" ^ Int.toString (#rank info) ^ "d"

fun futharkTypeToSML (FUTHARK_ARRAY info) = futharkArrayType info

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
    SOME t' => futharkTypeToSML t'
  | NONE => primTypeToSML t

fun blankRef "i8" = "Int8.fromInt 0"
  | blankRef "i16" = "Int16.fromInt 0"
  | blankRef "i32" = "Int32.fromInt 0"
  | blankRef "i64" = "Int64.fromInt 0"
  | blankRef "u8" = "Word8.fromInt 0"
  | blankRef "u16" = "Word16.fromInt 0"
  | blankRef "u32" = "Word32.fromInt 0"
  | blankRef "u64" = "Word64.fromInt 0"
  | blankRef "bool" = "false"
  | blankRef "[]i32" = "MLton.Pointer.null"
  | blankRef t =
      raise Fail ("blankRef: " ^ t)

fun generateEntrySpec manifest (name, entry_point {cfun, inputs, outputs}) =
  valspec ("entry_" ^ name) ("ctx" :: map (typeToSML manifest o #type_) inputs)
    (tuplify_t (map (typeToSML manifest o #type_) outputs))

fun mkSum [] = "0"
  | mkSum [x] = x
  | mkSum (x :: xs) = x ^ "+" ^ mkSum xs

fun mkSize (info: array_info) v =
  "let val shape_c = "
  ^
  fficall (#shape (#ops info)) [("ctx", "futhark_context"), (v, "pointer")]
    "pointer" ^ " in "
  ^
  mkSum (List.tabulate (#rank info, fn i =>
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
               SOME (FUTHARK_ARRAY _) => "(_, " ^ v ^ ")"
             | _ => v) :: inpParams (i + 1) rest
          end
    fun outDecs i [] = ""
      | outDecs i ({type_, unique = _} :: rest) =
          "val out" ^ Int.toString i ^ " = ref (" ^ blankRef type_ ^ ")" ^ "\n"
          ^ outDecs (i + 1) rest
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
               SOME (FUTHARK_ARRAY info) => tuplify_e ["ctx", "!" ^ v]
             | _ => "!" ^ v) :: outRes (i + 1) rest
          end
  in
    fundef ("entry_" ^ name) (["{cfg,ctx}"] @ (inpParams 0 inputs))
      ("let\n" ^ outDecs 0 outputs ^ "val ret = "
       ^
       fficall cfun
         ([("ctx", "futhark_context")] @ outArgs 0 outputs @ inpArgs 0 inputs)
         "Int32.int" ^ "\nin" ^ tuplify_e (outRes 0 outputs) ^ " end")
  end

fun shapeTypeOfRank d =
  (tuplify_t o replicate d) "int"

fun generateTypeSpec manifest (name, FUTHARK_ARRAY info) =
  unlines
    [ typespec (futharkArrayType info) []
    , valspec ("new_" ^ Int.toString (#rank info) ^ "d_" ^ #elemtype info)
        ["ctx", smlArrayType info, shapeTypeOfRank (#rank info)]
        (futharkArrayType info)
    , valspec ("shape_" ^ Int.toString (#rank info) ^ "d_" ^ (#elemtype info))
        [futharkArrayType info] (shapeTypeOfRank (#rank info))
    , valspec ("values_" ^ Int.toString (#rank info) ^ "d_" ^ (#elemtype info))
        [futharkArrayType info] (smlArrayType info)
    ]

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
      [ typedef (futharkArrayType info) []
          (tuplify_t ["futhark_context", "pointer"])
      , fundef ("new_" ^ Int.toString rank ^ "d_" ^ elemtype)
          [ "{ctx,cfg}"
          , parens ("data: " ^ data_t)
          , parens ("shape: " ^ shapeTypeOfRank rank)
          ]
          (tuplify_e
             [ "ctx"
             , fficall (#new ops)
                 ([("ctx", "futhark_context"), ("data", data_t)] @ shape_args)
                 "pointer"
             ])
      , fundef ("shape_" ^ Int.toString (#rank info) ^ "d_" ^ (#elemtype info))
          ["(ctx,data)"] (mkShape info "data")
      , fundef ("values_" ^ Int.toString rank ^ "d_" ^ elemtype) ["(ctx, data)"]
          (unlines
             [ "let"
             , "val n = " ^ mkSize info "data"
             , "val out = "
               ^ apply (smlArrayModule info ^ ".array") ["n", blankRef elemtype]
             , "val err = "
               ^
               fficall (#values ops)
                 [ ("ctx", "futhark_context")
                 , ("data", "pointer")
                 , ("out", data_t)
                 ] "Int32.int"
             , "in out end"
             ])
      ]
  end

fun generate (manifest as MANIFEST {backend, entry_points, types}) =
  let
    val type_cfg = typedef "cfg" [] "{}"
    val exn_fut = "exception futhark of string"
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
      ] @ type_specs @ entry_specs
    val defs =
      [ typedef "pointer" [] "MLton.Pointer.t"
      , typedef "ctx" [] "{cfg: pointer, ctx: pointer}"
      , exn_fut
      , type_cfg
      , typedef "futhark_context_config" [] "pointer"
      , typedef "futhark_context" [] "pointer"
      , "val default_cfg = {}"
      , fundef "ctx_new" ["{}"] (unlines
          [ "let"
          , "val c_cfg ="
          , fficall "futhark_context_config_new" [] "futhark_context_config"
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
    ( "signature FUTHARK = sig\n" ^ unlines specs ^ "end\n"
    , "structure Futhark :> FUTHARK = struct\n" ^ unlines defs ^ "end\n"
    )
  end

fun main () =
  case CommandLine.arguments () of
    [json_file] =>
      let
        val m = manifestFromFile json_file
        val (sig_s, mod_s) = generate m
        val base = Path.base json_file
      in
        writeFile (base ^ ".sig") sig_s;
        writeFile (base ^ ".sml") mod_s
      end
  | _ =>
      ( TextIO.output (TextIO.stdErr, "Need a Futhark manifest file.\n")
      ; Process.exit Process.failure
      )

val () = main ()
