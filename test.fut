entry main (x: u32) = x |> i64.u32 |> iota |> i64.sum

entry array_i32 (x: []i32) = map (+2) x
