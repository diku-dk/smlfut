type input = {name: string, type_: string, unique: bool}

type output = {type_: string, unique: bool}

datatype entry_point =
         entry_point of {cfun: string, inputs: input list, outputs: output list}

datatype manifest =
         MANIFEST of {backend: string, entry_points: (string * entry_point) list}


fun explainManifest (MANIFEST m) =
    ( print ("Entry points:\n")
    ; map (fn (s, _) => print (s ^ "\n")) (#entry_points m)
    ; ()
    )

fun writeFile fname s =
    let val os = TextIO.openOut fname
    in TextIO.output(os, s) before TextIO.closeOut os end

fun generate (MANIFEST {backend, entry_points}) =
    let val specs = []
        val defs = []
    in ("signature FUTHARK = sig\n" ^ concat specs ^ "end\n",
        "structure Futhark :> FUTHARK = struct\n" ^ concat defs ^ "end\n")
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
        let val m = manifestFromFile json_file
            val (sig_s, mod_s) = generate m
            val base = Path.base json_file
        in writeFile (base ^ ".sig") sig_s;
           writeFile (base ^ ".sml") mod_s
        end
      | _ =>
        ( TextIO.output (TextIO.stdErr, "Need a Futhark manifest file.\n")
        ; Process.exit Process.failure
        )

val () = main ()
