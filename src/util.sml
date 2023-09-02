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

fun valspec fname params ret =
  "val " ^ fname ^ " : " ^ punctuate " -> " (params @ [ret])

fun writeFile fname s =
  let val os = TextIO.openOut fname
  in TextIO.output (os, s) before TextIO.closeOut os
  end
