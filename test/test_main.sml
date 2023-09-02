val () =
    let val ctx = Futhark.ctx_new Futhark.default_cfg
        val x = Futhark.entry_main ctx 0w123
        val arr_in = Futhark.new_1d_i32 ctx (Array.fromList [1,2,3]) 3
        val arr_out = Futhark.entry_array_i32 ctx arr_in
        val () = Futhark.ctx_free ctx
    in print (Int64.toString x ^ "\n");
       print (Int.toString (Futhark.shape arr_in) ^ "\n");
       print (Int.toString (Futhark.shape arr_out) ^ "\n")
    end
