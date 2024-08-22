(* Generating an SML type name from a Futhark type expression. *)

fun constituent c =
  Char.isAlphaNum c orelse c = #"'" orelse c = #"_"

fun isValidName s =
  List.all constituent (explode s)

fun checkValidName s =
  if isValidName s then ()
  else raise Fail ("\"" ^ s ^ "\" is an invalid SML identifier.")

local
  datatype 'a parser = P of char list -> ('a * char list) option

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
      | SOME (x, s') => let val P f' = f x in f' s' end)

  val anyChar: char parser = P (fn s =>
    case s of
      [] => NONE
    | c :: s' => SOME (c, s'))

  fun accept x =
    P (fn s => SOME (x, s))

  val reject: 'a parser = P (fn s => NONE)

  val eof = P (fn s => if null s then SOME ((), s) else NONE)

  fun satisfy p =
    anyChar >>= (fn c => if p c then accept c else reject)

  fun char c =
    satisfy (fn c' => c = c')

  fun p1 <|> p2 =
    P (fn s =>
      let
        val P p1' = p1 ()
      in
        case p1' s of
          SOME x => SOME x
        | NONE => let val P p2' = p2 () in p2' s end
      end)

  fun choice [] = reject
    | choice (p :: ps) =
        p <|> (fn () => choice ps)

  fun p1 <*> p2 =
    p1 >>= (fn f => p2 >>= (fn x => accept (f x)))

  fun p1 <* p2 =
    accept (fn x => fn _ => x) <*> p1 <*> p2

  fun p1 *> p2 =
    accept (fn _ => fn x => x) <*> p1 <*> p2

  fun f <$> p =
    p >>= (fn x => accept (f x))

  fun many p =
    (fn () => (p >>= (fn x => many p >>= (fn xs => accept (x :: xs)))))
    <|> (fn () => accept [])

  fun some p =
    p >>= (fn x => many p >>= (fn xs => accept (x :: xs)))

  val space = many (satisfy (fn c => c = #" "))

  fun lexeme p = p <* space

  fun enclose lb rb p = lb *> p <* rb

  fun sepBy1 p sep =
    p
    >>=
    (fn x =>
       choice
         [ fn () => sep *> sepBy1 p sep >>= (fn xs => accept (x :: xs))
         , fn () => accept [x]
         ])

  fun sepBy p sep =
    choice [fn () => sepBy1 p sep, fn () => accept []]

  fun lChar c =
    lexeme (char c)

  fun lString s =
    let
      fun loop [] = accept ()
        | loop (c :: cs) = char c *> loop cs
    in
      lexeme (loop (explode s))
    end

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

  (* Futhark type. *)
  datatype T =
    TVar of string
  | TArray of T
  | TTuple of T list
  | TRecord of (string * T) list

  fun pT () =
    choice
      [ fn () => TVar <$> lName
      , fn () => parens (pT ())
      , fn () => TArray <$> (lString "[]" *> pT ())
      , fn () => TTuple <$> parens (sepBy (pT ()) (lChar #","))
      , fn () => TRecord <$> braces (sepBy (pField ()) (lChar #","))
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
