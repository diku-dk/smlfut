type input = {name: string, type_: string, unique: bool}

type output = {type_: string, unique: bool}

datatype entry_point =
  entry_point of {cfun: string, inputs: input list, outputs: output list}

type array_info =
  { ctype: string
  , elemtype: string
  , rank: int
  , ops: {free: string, new: string, shape: string}
  }

datatype futhark_type = FUTHARK_ARRAY of array_info

datatype manifest =
  MANIFEST of
    { backend: string
    , entry_points: (string * entry_point) list
    , types: (string * futhark_type) list
    }

fun explainManifest (MANIFEST m) =
  ( print ("Entry points:\n")
  ; map (fn (s, _) => print (s ^ "\n")) (#entry_points m)
  ; ()
  )

fun lookupType t (MANIFEST m) =
  case List.find (fn (t', _) => t = t') (#types m) of
    SOME (_, info) => SOME info
  | NONE => NONE

fun writeFile fname s =
  let val os = TextIO.openOut fname
  in TextIO.output (os, s) before TextIO.closeOut os
  end

val unlines = concat o map (fn s => s ^ "\n")

fun parens s = "(" ^ s ^ ")"

fun intersperse y [] = []
  | intersperse y [x] = [x]
  | intersperse y (x :: xs) =
      x :: y :: intersperse y xs

fun punctuate c = concat o intersperse c

fun replicate 0 x = []
  | replicate n x =
      x :: replicate (n - 1) x

fun tuplify_e [x] = x
  | tuplify_e [] = "()"
  | tuplify_e xs =
      parens (punctuate "," xs)

fun tuplify_t [x] = x
  | tuplify_t [] = "unit"
  | tuplify_t xs =
      parens (punctuate "*" xs)

fun fundef fname args body =
  "fun " ^ fname ^ " " ^ punctuate " " args ^ " =\n" ^ body

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

fun typeToSML "[]i32" = "(int shape,Int32.int) array"
  | typeToSML t =
      case isPrimType t of
        SOME t' => t'
      | NONE => raise Fail ("Cannot map type to SML: " ^ t)

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

fun generateEntrySpec (name, entry_point {cfun, inputs, outputs}) =
  let
    fun inpParam {name = _, type_, unique = _} = " -> " ^ typeToSML type_
    fun outRes {type_, unique = _} = typeToSML type_
  in
    ("val entry_" ^ name ^ " : ctx" ^ concat (map inpParam inputs) ^ " -> "
     ^ tuplify_t (map outRes outputs))
  end

fun entryImport (entry_point {cfun, inputs, outputs}) =
  let
    fun apiType t =
      case isPrimType t of
        SOME t' => t'
      | NONE => "pointer"
    fun outArgType out =
      "* " ^ apiType (#type_ out) ^ " ref"
    fun inpArgType inp =
      "* " ^ apiType (#type_ inp)
  in
    "(_import \"" ^ cfun ^ "\" public : futhark_context "
    ^ concat (map outArgType outputs) ^ concat (map inpArgType inputs)
    ^ " -> Int32.int;)"
  end

fun mkShape (info: array_info) v =
  "let val shape_c = (_import \"" ^ #shape (#ops info)
  ^ "\" public : futhark_context * pointer -> pointer;)" ^ tuplify_e ["ctx", v]
  ^ " in "
  ^
  tuplify_e (List.tabulate (#rank info, fn i =>
    "Int64.toInt"
    ^ parens ("MLton.Pointer.getInt64" ^ tuplify_e ["shape_c", Int.toString i])))
  ^ " end"

fun generateEntryDef manifest (name, ep as entry_point {cfun, inputs, outputs}) =
  let
    fun inpParams i [] = []
      | inpParams i ({name = _, type_, unique = _} :: rest) =
          let
            val v = "inp" ^ Int.toString i
          in
            (case lookupType type_ manifest of
               SOME (FUTHARK_ARRAY _) => "(_, " ^ v ^ ")"
             | _ => v) :: inpParams (i + 1) rest
          end
    fun outDecs i [] = ""
      | outDecs i ({type_, unique = _} :: rest) =
          "val out" ^ Int.toString i ^ " = ref (" ^ blankRef type_ ^ ")" ^ "\n"
          ^ outDecs (i + 1) rest
    fun outArgs i [] = []
      | outArgs i (out :: rest) =
          ("out" ^ Int.toString i) :: outArgs (i + 1) rest
    fun inpArgs i [] = []
      | inpArgs i (inp :: rest) =
          "inp" ^ Int.toString i :: inpArgs (i + 1) rest
    fun outRes i [] = []
      | outRes i (out :: rest) =
          let
            val v = "out" ^ Int.toString i
          in
            (case lookupType (#type_ out) manifest of
               SOME (FUTHARK_ARRAY info) =>
                 tuplify_e [mkShape info ("!" ^ v), "!" ^ v]
             | _ => "!" ^ v) :: outRes (i + 1) rest
          end
  in
    fundef ("entry_" ^ name) (["{cfg,ctx}"] @ (inpParams 0 inputs))
      ("let\n" ^ outDecs 0 outputs ^ "val ret = " ^ entryImport ep
       ^ tuplify_e (["ctx"] @ outArgs 0 outputs @ inpArgs 0 inputs) ^ "\nin"
       ^ tuplify_e (outRes 0 outputs) ^ " end")
  end

fun shapeTypeOfRank d =
  (tuplify_t o replicate d) "int" ^ " shape"

fun generateTypeDef (name, FUTHARK_ARRAY {ctype, rank, elemtype, ops}) =
  let
    val data_t = typeToSML elemtype ^ " Array.array"
  in
    unlines
      [fundef ("new_" ^ Int.toString rank ^ "d_" ^ elemtype)
         [ "{ctx,cfg}"
         , parens ("data: " ^ data_t)
         , parens ("shape: " ^ shapeTypeOfRank rank)
         ]
         (tuplify_e
            [ "shape"
            , "(_import \"" ^ #new ops ^ "\" : futhark_context * " ^ data_t
              ^ " * " ^ punctuate "*" (replicate rank "Int64.int")
              ^ " -> pointer;)"
              ^
              tuplify_e
                (["ctx", "data"]
                 @
                 (if rank = 1 then
                    ["Int64.fromInt shape"]
                  else
                    List.tabulate (rank, fn i =>
                      "Int64.fromInt"
                      ^ parens ("#" ^ Int.toString (i + 1) ^ " shape"))))
            ])]
  end

fun generateTypeSpec (name, FUTHARK_ARRAY {ctype, rank, elemtype, ops}) =
  unlines
    ["val new_" ^ Int.toString rank ^ "d_" ^ elemtype ^ " : ctx -> "
     ^ typeToSML elemtype ^ " Array.array -> " ^ shapeTypeOfRank rank ^ " -> "
     ^ tuplify_e [shapeTypeOfRank rank, typeToSML elemtype] ^ " array"]

fun generate (manifest as MANIFEST {backend, entry_points, types}) =
  let
    val type_cfg = "type cfg = {}"
    val type_shape = "type 'a shape = 'a"
    val exn_fut = "exception futhark of string"
    val entry_specs = map generateEntrySpec entry_points
    val entry_defs = map (generateEntryDef manifest) entry_points
    val type_specs = map generateTypeSpec types
    val type_defs = map generateTypeDef types
    val specs =
      [ "type ctx"
      , exn_fut
      , type_cfg
      , "val default_cfg : cfg"
      , "val ctx_new : cfg -> ctx"
      , "val ctx_free : ctx -> unit"
      , type_shape
      , "type ('elem, 'shape) array"
      , "val shape : ('elem, 'shape) array -> 'shape"
      ] @ type_specs @ entry_specs
    val defs =
      [ "type pointer = MLton.Pointer.t"
      , "type ctx = {cfg: pointer, ctx: pointer}"
      , exn_fut
      , type_cfg
      , "type futhark_context_config = pointer"
      , "type futhark_context = pointer"
      , "val default_cfg = {}"
      , fundef "ctx_new" ["{}"] (unlines
          [ "let"
          , "val c_cfg ="
          , "(_import \"futhark_context_config_new\" public : unit -> futhark_context_config;) ()"
          , "val c_ctx ="
          , "(_import \"futhark_context_new\" public : futhark_context_config -> futhark_context;) c_cfg"
          , "in {cfg=c_cfg, ctx=c_ctx} end"
          ])
      , fundef "ctx_free" ["{cfg,ctx}"] (unlines
          [ "let"
          , "val () = (_import \"futhark_context_free\" public : futhark_context -> unit;) ctx"
          , "val () = (_import \"futhark_context_config_free\" public : futhark_context_config -> unit;) cfg"
          , "in () end"
          ])
      , type_shape
      , "type ('elem, 'shape) array = 'shape * pointer"
      , fundef "shape" ["(x,_)"] "x"
      ] @ type_defs @ entry_defs
  in
    ( "signature FUTHARK = sig\n" ^ unlines specs ^ "end\n"
    , "structure Futhark :> FUTHARK = struct\n" ^ unlines defs ^ "end\n"
    )
  end

local
  fun lookBool obj k =
    case Json.objLook obj k of
      SOME (Json.BOOL b) => b
    | _ => raise Fail ("Missing key: " ^ k)

  fun lookInt obj k =
    case Json.objLook obj k of
      SOME (Json.NUMBER s) =>
        (case Int.fromString s of
           SOME x => x
         | NONE => raise Fail ("Not an int: " ^ s))
    | _ => raise Fail ("Missing key: " ^ k)

  fun lookString obj k =
    case Json.objLook obj k of
      SOME (Json.STRING s) => s
    | _ => raise Fail ("Missing key: " ^ k)

  fun lookArray obj k =
    case Json.objLook obj k of
      SOME (Json.ARRAY l) => l
    | _ => raise Fail ("Missing key: " ^ k)

  fun lookObj obj k =
    case Json.objLook obj k of
      SOME (Json.OBJECT obj) => obj
    | _ => raise Fail ("Missing key: " ^ k)

  fun inputFromJSON (Json.OBJECT obj) : input =
        { name = lookString obj "name"
        , type_ = lookString obj "type"
        , unique = lookBool obj "unique"
        }
    | inputFromJSON _ = raise Fail "Invalid input in manifest"

  fun outputFromJSON (Json.OBJECT obj) : output =
        {type_ = lookString obj "type", unique = lookBool obj "unique"}
    | outputFromJSON _ = raise Fail "Invalid output in manifest"

  fun entryPointFromJSON (name, Json.OBJECT obj) =
        ( name
        , entry_point
            { cfun = lookString obj "cfun"
            , inputs = map inputFromJSON (lookArray obj "inputs")
            , outputs = map outputFromJSON (lookArray obj "outputs")
            }
        )
    | entryPointFromJSON _ =
        raise Fail "Invalid entry point in manifest."

  fun arrayOps obj =
    { free = lookString obj "free"
    , new = lookString obj "new"
    , shape = lookString obj "shape"
    }

  fun typeFromJSON (name, Json.OBJECT obj) =
        ( name
        , case lookString obj "kind" of
            "array" =>
              FUTHARK_ARRAY
                { ctype = lookString obj "ctype"
                , elemtype = lookString obj "elemtype"
                , rank = lookInt obj "rank"
                , ops = arrayOps (lookObj obj "ops")
                }
          | kind => raise Fail ("Cannot handle type of kind: " ^ kind)
        )
    | typeFromJSON _ = raise Fail "Invalid type in manifest."

  fun manifestFromJSON (Json.OBJECT obj) =
        let
          fun get k [] = raise Domain
            | get k ((x, v) :: rest) =
                if k = x then v else get k rest
        in
          MANIFEST
            { backend = lookString obj "backend"
            , entry_points =
                case Json.objLook obj "entry_points" of
                  SOME (Json.OBJECT entry_obj) =>
                    map entryPointFromJSON (Json.objList entry_obj)
                | _ => raise Fail "Invalid entry points in manifest."
            , types =
                case Json.objLook obj "types" of
                  SOME (Json.OBJECT types_obj) =>
                    map typeFromJSON (Json.objList types_obj)
                | _ => raise Fail "Invalid types in manifest."
            }
        end
    | manifestFromJSON json =
        raise Fail "Manifest is not a JSON object."
in
  val manifestFromFile =
    manifestFromJSON o Json.fromString o TextIO.inputAll o TextIO.openIn
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
