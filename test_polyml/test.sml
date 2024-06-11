local val lib = Foreign.loadExecutable ()
in
  val futhark_context_config_new = Foreign.buildCall0
    (Foreign.getSymbol lib "futhark_context_config_new", (), Foreign.cPointer)
  val futhark_context_new = Foreign.buildCall1
    ( Foreign.getSymbol lib "futhark_context_new"
    , Foreign.cPointer
    , Foreign.cPointer
    )
  val futhark_context_config_free = Foreign.buildCall1
    ( Foreign.getSymbol lib "futhark_context_config_free"
    , Foreign.cPointer
    , Foreign.cVoid
    )
  val futhark_context_free = Foreign.buildCall1
    ( Foreign.getSymbol lib "futhark_context_free"
    , Foreign.cPointer
    , Foreign.cVoid
    )
  val futhark_entry_main = Foreign.buildCall3
    ( Foreign.getSymbol lib "futhark_entry_main"
    , (Foreign.cPointer, Foreign.cPointer, Foreign.cUint32)
    , Foreign.cInt
    )
end
