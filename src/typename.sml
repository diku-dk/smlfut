(* Generating an SML type name from a Futhark type expression. *)

fun constituent c =
  Char.isAlphaNum c orelse c = #"'" orelse c = #"_"

fun isValidName s =
  List.all constituent (explode s)

fun checkValidName s =
  if isValidName s then ()
  else raise Fail ("\"" ^ s ^ "\" is an invalid SML identifier.")

structure Parser:
sig
  type 'a p
  val parse: 'a p -> string -> 'a option
  val delay: ('a -> 'b p) -> 'a -> 'b p
  val satisfy: (char -> bool) -> char p
  val accept: 'a -> 'a p
  val reject: 'a p
  val eof: unit p
  val >>= : 'a p * ('a -> 'b p) -> 'b p
  val <|> : 'a p * 'a p -> 'a p
  val <* : 'a p * 'b p -> 'a p
  val *> : 'a p * 'b p -> 'b p
  val <*> : ('a -> 'b) p * 'a p -> 'b p
  val <$> : ('a -> 'b) * 'a p -> 'b p
end =
struct
  datatype 'a p = P of char list -> ('a * char list) option

  fun runP (P f) = f

  fun parse (P f) s =
    case f (explode s) of
      SOME (x, s') => SOME x
    | NONE => NONE

  infixr >>= <|> *> <*
  infix <*> <$>
  fun (P p) >>= f =
    P (fn s =>
      case p s of
        NONE => NONE
      | SOME (x, s') => runP (f x) s')

  val anyChar: char p = P (fn s =>
    case s of
      [] => NONE
    | c :: s' => SOME (c, s'))

  fun accept x =
    P (fn s => SOME (x, s))

  val reject: 'a p = P (fn s => NONE)

  fun delay (fp: 'a -> 'b p) (x: 'a) : 'b p =
    P (fn s => runP (fp x) s)

  val eof = P (fn s => if null s then SOME ((), s) else NONE)

  fun satisfy p =
    anyChar >>= (fn c => if p c then accept c else reject)

  fun (P p1) <|> (P p2) =
    P (fn s =>
      case p1 s of
        SOME x => SOME x
      | NONE => p2 s)

  fun p1 <*> p2 =
    p1 >>= (fn f => p2 >>= (fn x => accept (f x)))

  fun p1 <* p2 =
    accept (fn x => fn _ => x) <*> p1 <*> p2

  fun p1 *> p2 =
    accept (fn _ => fn x => x) <*> p1 <*> p2

  fun f <$> p =
    p >>= (fn x => accept (f x))
end

local
  open Parser

  infixr >>= <|> *> <*
  infix <*> <$>

  fun char c =
    satisfy (fn c' => c = c')

  fun many p =
    (p >>= (fn x => many p >>= (fn xs => accept (x :: xs)))) <|> delay accept []

  val space = many (satisfy (fn c => c = #" "))

  fun lexeme p = p <* space

  fun lChar c =
    lexeme (char c)

  fun lString s =
    let
      fun loop [] = accept ()
        | loop (c :: cs) = char c *> loop cs
    in
      lexeme (loop (explode s))
    end


  fun some p =
    p >>= (fn x => many p >>= (fn xs => accept (x :: xs)))

  fun enclose lb rb p = lb *> p <* rb

  fun choice [] = reject
    | choice (p :: ps) =
        p <|> delay choice ps

  fun sepBy1 p sep =
    p
    >>=
    (fn x =>
       choice
         [ sep *> delay (sepBy1 p) sep >>= (fn xs => accept (x :: xs))
         , delay accept [x]
         ])

  fun sepBy p sep =
    choice [delay (sepBy1 p) sep, delay accept []]


  fun parens p =
    enclose (lChar #"(") (lChar #")") p

  fun braces p =
    enclose (lChar #"{") (lChar #"}") p

  fun constituent c = Char.isAlpha c

  local
    val firstChar = satisfy Char.isAlpha
    val secondChar = satisfy (fn c => Char.isAlphaNum c orelse c = #"'")
  in
    val lName = lexeme
      ((fn c => fn cs => implode (c :: cs)) <$> firstChar <*> many secondChar)
  end

  fun delay0 f = delay f ()

  (* Futhark type. *)
  datatype T =
    TVar of string
  | TArray of T
  | TTuple of T list
  | TRecord of (string * T) list

  fun pT () =
    choice
      [ TVar <$> lName
      , parens (delay0 pT)
      , TArray <$> (lString "[]" *> delay0 pT)
      , TTuple <$> parens (sepBy (delay0 pT) (lChar #","))
      , TRecord <$> braces (sepBy (delay0 pField) (lChar #","))
      ]
  and pField () =
    (fn v => fn t => (v, t)) <$> (lName <* lChar #":") <*> pT ()

  fun showT (TVar s) = s
    | showT (TArray t) = "[]" ^ showT t
    | showT (TTuple ts) =
        "(" ^ punctuate "," (map showT ts) ^ ")"
    | showT (TRecord ts) =
        "{" ^ punctuate "," (map (fn (v, t) => v ^ ":" ^ showT t) ts) ^ "}"

  (* Make a string a valid SML identifier, whatever it may presently be. *)
  fun escapeName name =
    let
      fun escape c =
        if constituent c then
          str c
        else
          case c of
            #"[" => "_LB_"
          | #"]" => "_RB_"
          | _ => "_"
      val name' = concat (map escape (explode name))
    in
      if name <> name' then "unrep_" ^ name' else name
    end

  fun showTSML (TVar s) = s
    | showTSML (TArray t) = "arr_" ^ showTSML t
    | showTSML (TTuple ts) =
        "tup" ^ Int.toString (length ts) ^ "_" ^ punctuate "_" (map showTSML ts)
    | showTSML (TRecord ts) =
        "rec" ^ Int.toString (length ts) ^ "_" ^ punctuate "_" (map #1 ts)
in
  (*
    val () =
      case
        parse ((space *> pT ()) <* eof) (List.nth (CommandLine.arguments (), 0))
      of
        SOME x => print (showTSML x)
      | NONE => print "failed"
  *)

  fun futTypeName s =
    case parse ((space *> pT ()) <* eof) s of
      SOME x => showTSML x
    | NONE => escapeName s
end
