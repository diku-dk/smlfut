val () =
    let val ctx = Futhark.ctx_new Futhark.default_cfg
        val x = Futhark.entry_main ctx 0w123
        val () = Futhark.ctx_free ctx
    in print (Word32.toString x ^ "\n")
    end
