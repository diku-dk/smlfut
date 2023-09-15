entry main (x: u32) = x |> i64.u32 |> iota |> i64.sum

entry array_i32 (x: []i32) = map (+2) x ++ x

entry array_f64 (x: []f64) = map (+2) x ++ x

entry transpose_i32 (x: [][]i32) = transpose x

entry fails (x: i32) = 2/x

type record = {a: i32, b: bool}

entry mk_record (a: i32) (b: bool) : record = {a,b}

type sum_opaque = #foo i32

entry mk_sum_opaque x : sum_opaque = #foo x
