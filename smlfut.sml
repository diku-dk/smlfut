type input = {name: string, type_: string, unique: bool}

type output = {type_: string, unique: bool}

datatype entry_point =
  entry_point of {cfun: string, inputs: input list, outputs: output list}

datatype futhark_type =
  FUTHARK_ARRAY of
    { ctype: string
    , elemtype: string
    , ops: {free: string, new: string, shape: string}
    }

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

fun writeFile fname s =
  let val os = TextIO.openOut fname
  in TextIO.output (os, s) before TextIO.closeOut os
  end

val unlines = concat o map (fn s => s ^ "\n")

fun parens s = "(" ^ s ^ ")"

fun intersperse y [] = []
  | intersperse y [x] = [x]
  | intersperse y (x::xs) = x :: y :: intersperse y xs

fun punctuate c = concat o intersperse c

fun replicate 0 x = []
  | replicate n x = x :: replicate (n-1) x

fun tuplify_e [x] = x
  | tuplify_e [] = "()"
  | tuplify_e xs = parens (punctuate "," xs)

fun tuplify_t [x] = x
  | tuplify_t [] = "unit"
  | tuplify_t xs = parens (punctuate "*" xs)

fun typeToSML "i8" = "Int8.int"
  | typeToSML "i16" = "Int16.int"
  | typeToSML "i32" = "Int32.int"
  | typeToSML "i64" = "Int64.int"
  | typeToSML "u8" = "Word8.word"
  | typeToSML "u16" = "Word16.word"
  | typeToSML "u32" = "Word32.word"
  | typeToSML "u64" = "Word64.word"
  | typeToSML "bool" = "bool"
  | typeToSML "[]i32" = "(int shape,Int32.int) array"
  | typeToSML t =
      raise Fail ("Cannot map type to SML: " ^ t)

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
    fun outArgType out =
      "* " ^ typeToSML (#type_ out) ^ " ref"
    fun inpArgType inp =
      "* " ^ typeToSML (#type_ inp)
  in
    "(_import \"" ^ cfun ^ "\" public : futhark_context "
    ^ concat (map outArgType outputs) ^ concat (map inpArgType inputs)
    ^ " -> Int32.int;)"
  end

fun generateEntryDef (name, ep as entry_point {cfun, inputs, outputs}) =
  let
    fun inpParams i [] = ""
      | inpParams i ({name = _, type_, unique = _} :: rest) =
          " (inp" ^ Int.toString i ^ ":" ^ typeToSML type_ ^ ")"
          ^ inpParams (i + 1) rest
    fun outDecs i [] = ""
      | outDecs i ({type_, unique = _} :: rest) =
          "val out" ^ Int.toString i ^ " = ref (" ^ blankRef type_ ^ ")" ^ "\n"
          ^ outDecs (i + 1) rest
    fun outArgs i [] = ""
      | outArgs i (out :: rest) =
          ",out" ^ Int.toString i ^ outArgs (i + 1) rest
    fun inpArgs i [] = ""
      | inpArgs i (inp :: rest) =
          ",inp" ^ Int.toString i ^ inpArgs (i + 1) rest
    fun outRes i [] = []
      | outRes i (_ :: rest) =
          ("!out" ^ Int.toString i) :: outRes (i + 1) rest
  in
    ("fun entry_" ^ name ^ " {cfg,ctx}" ^ inpParams 0 inputs ^ " = let\n"
     ^ outDecs 0 outputs ^ "val ret = " ^ entryImport ep ^ " (ctx"
     ^ outArgs 0 outputs ^ inpArgs 0 inputs ^ ")\n" ^ "in "
     ^ tuplify_e (outRes 0 outputs) ^ " end")
  end

fun shapeTypeOfRank d =
    (tuplify_t o replicate d) "int" ^ " shape"

val determineRank =
    let fun f (#"[" :: #"]" :: rest) = 1 + f rest
          | f _ = 0
    in f o explode end

fun generateTypeDef (name, FUTHARK_ARRAY {ctype, elemtype, ops}) =
    let val d = determineRank name
        val data_t = typeToSML elemtype ^ " Array.array"
    in unlines [
            "fun new_" ^ Int.toString d ^ "d_" ^ elemtype ^ " {ctx,cfg} " ^
            parens ("data: " ^ data_t) ^
            punctuate " " (List.tabulate (d, fn i => parens ("d" ^ Int.toString i ^ ": " ^ shapeTypeOfRank d))) ^ " =",
            "(_import \"" ^ #new ops ^ "\" : futhark_context * " ^
            data_t ^ " * " ^
            punctuate "*" (replicate d "Int64.int") ^ " -> MLton.Pointer.t;)" ^
            tuplify_e (["ctx", "data"] @
                       List.tabulate (d, fn i => "Int64.fromInt d" ^ Int.toString i))
        ]
    end

fun generateTypeSpec (name, FUTHARK_ARRAY {ctype, elemtype, ops}) =
    let val d = determineRank name
    in unlines [
            "val new_" ^ Int.toString d ^ "d_" ^ elemtype ^ " : ctx -> " ^
        typeToSML elemtype ^ " Array.array -> " ^
        shapeTypeOfRank d ^ " -> " ^
        tuplify_e [shapeTypeOfRank d, typeToSML elemtype] ^ " array"
    ] end

fun generate (MANIFEST {backend, entry_points, types}) =
  let
    val type_cfg = "type cfg = {}"
    val type_shape = "type 'a shape = 'a"
    val exn_fut = "exception futhark of string"
    val entry_specs = map generateEntrySpec entry_points
    val entry_defs = map generateEntryDef entry_points
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
      ] @ type_specs @ entry_specs
    val defs =
      [ "type ctx = {cfg: MLton.Pointer.t, ctx: MLton.Pointer.t}"
      , exn_fut
      , type_cfg
      , "type futhark_context_config = MLton.Pointer.t"
      , "type futhark_context = MLton.Pointer.t"
      , "val default_cfg = {}"
      , "fun ctx_new {} = let"
      , "val c_cfg ="
      , "(_import \"futhark_context_config_new\" public : unit -> futhark_context_config;) ()"
      , "val c_ctx ="
      , "(_import \"futhark_context_new\" public : futhark_context_config -> futhark_context;) c_cfg"
      , "in {cfg=c_cfg, ctx=c_ctx} end"
      , "fun ctx_free {cfg,ctx} = let"
      , "val () = (_import \"futhark_context_free\" public : futhark_context -> unit;) ctx"
      , "val () = (_import \"futhark_context_config_free\" public : futhark_context_config -> unit;) cfg"
      , "in () end"
      , type_shape
      , "type ('elem, 'shape) array = MLton.Pointer.t"
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
