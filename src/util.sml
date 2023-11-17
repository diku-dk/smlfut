val unlines = concat o map (fn s => s ^ "\n")
val unwords = concat o map (fn s => s ^ " ")

fun parens s = "(" ^ s ^ ")"
fun braces s = "{" ^ s ^ "}"
fun indent "" = ""
  | indent s = "  " ^ s

fun intersperse y [] = []
  | intersperse y [x] = [x]
  | intersperse y (x :: xs) =
      x :: y :: intersperse y xs

fun punctuate c = concat o intersperse c

fun replicate 0 x = []
  | replicate n x =
      x :: replicate (n - 1) x

fun tuple_e [x] = x
  | tuple_e [] = "()"
  | tuple_e xs =
      parens (punctuate ", " xs)

fun tuple_t [x] = x
  | tuple_t [] = "unit"
  | tuple_t xs = punctuate " * " xs

fun apply f args =
  f ^ parens (punctuate ", " args)

fun tapply f [] = f
  | tapply f args =
      parens (punctuate ", " args) ^ f

fun record_e fs =
  braces (punctuate ", " (map (fn (f, t) => f ^ "=" ^ t) fs))

fun record_t fs =
  braces (punctuate ", " (map (fn (f, t) => f ^ ":" ^ t) fs))

fun stringlit s =
  let
    fun escape #"\n" = "\\n"
      | escape #"\"" = "\\\""
      | escape c = str c
  in
    "\"" ^ String.translate escape s ^ "\""
  end

fun fundef fname args body =
  ["fun " ^ fname ^ " " ^ punctuate " " args ^ " ="] @ map indent body

fun valdef vname rhs =
  "val " ^ vname ^ " = " ^ rhs

fun valspec fname params ret =
  "val " ^ fname ^ " : " ^ punctuate " -> " (params @ [ret])

fun typespec tname args =
  "type " ^ tapply tname args

fun typedef tname args def =
  "type " ^ tapply tname args ^ " = " ^ def

fun datatypedef tname args def =
  "datatype " ^ tapply tname args ^ " = " ^ def

fun letbind binds body =
  ["let"] @ map (fn (p, e) => "val " ^ p ^ " = " ^ e) binds @ ["in"] @ body
  @ ["end"]

fun sigexp specs =
  ["sig"] @ map indent specs @ ["end"]

fun sigdef name specs =
  ["signature " ^ name ^ " = sig"] @ map indent specs @ ["end"]

fun structspec name sige =
  "structure " ^ name ^ " : " ^ sige

fun structdef name (SOME t) specs =
      ["structure " ^ name ^ ":>" ^ t ^ " = struct"] @ map indent specs
      @ ["end"]
  | structdef name NONE specs =
      ["structure " ^ name ^ " = struct"] @ map indent specs @ ["end"]

fun writeFile fname s =
  let val os = TextIO.openOut fname
  in TextIO.output (os, s) before TextIO.closeOut os
  end
