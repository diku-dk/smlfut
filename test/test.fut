entry main (x: u32) = x |> i64.u32 |> iota |> i64.sum

entry array_i32 (x: []i32) = map (+2) x ++ x

entry array_f64 (x: []f64) = map (+2) x ++ x

entry transpose_i32 (x: [][]i32) = transpose x

entry fails (x: i32) = 2/x
