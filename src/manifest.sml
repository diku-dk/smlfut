type input = {name: string, type_: string, unique: bool}

type output = {type_: string, unique: bool}

datatype entry_point =
  entry_point of
    { cfun: string
    , inputs: input list
    , outputs: output list
    , tuning_params: string list
    }

type array_info =
  { ctype: string
  , elemtype: string
  , rank: int
  , ops: { free: string
         , new: string
         , new_raw: string
         , shape: string
         , index: string
         , values: string
         , values_raw: string
         }
  }

type field = string * {project: string, type_: string}

type record = {fields: field list, new: string}

type variant =
  string * {construct: string, destruct: string, payload: string list}

type sum = {variants: variant list, variant: string}

type opaque_array = {rank: int, elemtype: string, index: string, shape: string}

type record_array =
  { rank: int
  , elemtype: string
  , fields: field list
  , zip: string
  , index: string
  , shape: string
  }

datatype opaque_extra =
  OPAQUE_RECORD of record
| OPAQUE_SUM of sum
| OPAQUE_ARRAY of opaque_array
| OPAQUE_RECORD_ARRAY of record_array

type opaque_info =
  { ctype: string
  , ops: {free: string, store: string, restore: string}
  , extra: opaque_extra option
  }

datatype futhark_type =
  FUTHARK_ARRAY of array_info
| FUTHARK_OPAQUE of opaque_info

datatype manifest =
  MANIFEST of
    { backend: string
    , version: string
    , entry_points: (string * entry_point) list
    , types: (string * futhark_type) list
    }

fun explainManifest (MANIFEST m) =
  ( print ("Entry points:\n")
  ; map (fn (s, _) => print (s ^ "\n")) (#entry_points m)
  ; ()
  )

fun lookupType (MANIFEST m) t =
  case List.find (fn (t', _) => t = t') (#types m) of
    SOME (_, info) => SOME info
  | NONE => NONE

local
  fun stringFromJSON (Json.STRING s) = s
    | stringFromJSON _ = raise Fail "Not a string"

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
            , tuning_params = map stringFromJSON (lookArray obj "tuning_params")
            }
        )
    | entryPointFromJSON _ =
        raise Fail "Invalid entry point in manifest."

  fun arrayOps obj =
    { free = lookString obj "free"
    , new = lookString obj "new"
    , shape = lookString obj "shape"
    , index = lookString obj "index"
    , values = lookString obj "values"
    , values_raw = lookString obj "values_raw"
    , new_raw = lookString obj "new_raw"
    }

  fun opaqueOps obj =
    { free = lookString obj "free"
    , store = lookString obj "store"
    , restore = lookString obj "restore"
    }

  fun fieldFromJSON (Json.OBJECT obj) =
        ( lookString obj "name"
        , {type_ = lookString obj "type", project = lookString obj "project"}
        )
    | fieldFromJSON _ = raise Fail "Invalid field in manifest."


  fun variantFromJSON (Json.OBJECT obj) =
        ( lookString obj "name"
        , { payload = map stringFromJSON (lookArray obj "payload")
          , construct = lookString obj "construct"
          , destruct = lookString obj "destruct"
          }
        )
    | variantFromJSON _ =
        raise Fail "Invalid variant in manifest."

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
          | "opaque" =>
              FUTHARK_OPAQUE
                { ctype = lookString obj "ctype"
                , ops = opaqueOps (lookObj obj "ops")
                , extra =
                    case
                      ( Json.objLook obj "record"
                      , Json.objLook obj "sum"
                      , Json.objLook obj "opaque_array"
                      , Json.objLook obj "record_array"
                      )
                    of
                      (SOME (Json.OBJECT robj), _, _, _) =>
                        SOME (OPAQUE_RECORD
                          { new = lookString robj "new"
                          , fields = (map fieldFromJSON
                              (lookArray robj "fields"))
                          })
                    | (SOME _, _, _, _) =>
                        raise Fail "Invalid record in manifest."
                    | (_, SOME (Json.OBJECT sobj), _, _) =>
                        SOME (OPAQUE_SUM
                          { variant = lookString sobj "variant"
                          , variants = map variantFromJSON
                              (lookArray sobj "variants")
                          })
                    | (_, SOME _, _, _) =>
                        raise Fail "Invalid record in manifest."
                    | (_, _, SOME (Json.OBJECT obj), _) =>
                        SOME (OPAQUE_ARRAY
                          { rank = lookInt obj "rank"
                          , elemtype = lookString obj "elemtype"
                          , index = lookString obj "index"
                          , shape = lookString obj "shape"
                          })
                    | (_, _, SOME _, _) =>
                        raise Fail "Invalid opaque array in manifest."
                    | (_, _, _, SOME (Json.OBJECT obj)) =>
                        SOME (OPAQUE_RECORD_ARRAY
                          { rank = lookInt obj "rank"
                          , elemtype = lookString obj "elemtype"
                          , index = lookString obj "index"
                          , fields = (map fieldFromJSON
                              (lookArray obj "fields"))
                          , shape = lookString obj "shape"
                          , zip = lookString obj "zip"
                          })
                    | (_, _, _, SOME _) =>
                        raise Fail "Invalid record array in manifest."
                    | _ => NONE
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
            , version = lookString obj "version"
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
