structure Futhark = test

val () =
  let
    val ctx = Futhark.Context.new
      (Futhark.Config.cache (SOME "futhark.cache") Futhark.Config.default)
  in
    Futhark.Context.free ctx
  end
