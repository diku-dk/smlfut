entry main (x: u32) = x |> i64.u32 |> iota |> i64.sum

entry array_i32 (x: []i32) = map (+2) x ++ x

entry array_f64 (x: []f64) = map (+2) x ++ x

entry transpose_i32 (x: [][]i32) = transpose x

entry fails (x: i32) = 2/x

type record = {a: i32, b: bool}

entry mk_record (a: i32) (b: bool) : record = {a,b}

type sum_opaque [n] = #foo i32 | #bar record | #baz ([n]i32) bool

entry mk_sum_opaque x : sum_opaque [42] = #foo x

entry sum_opaque_size [n] (_: sum_opaque [n]) = n

entry sum_opaque_rot (s: sum_opaque []) : ?[n].sum_opaque [n] =
  match s case #foo x -> #bar {a=x, b=true}
          case #bar {a,b} -> #baz (replicate 10 a) b
          case #baz arr _ -> #foo (i32.sum arr)

type~ record_with_opaque = {f: sum_opaque []}
entry record_with_opaque x : record_with_opaque = {f = mk_sum_opaque x}

entry mk_record_array (xs: []i32) (ys: []i32) = zip xs (zip ys ys)

entry mk_record_array_array (xs: [][]i32) (ys: []i32) = zip xs ys

entry mk_sum_unnamed (x: i32) = #foo x : sum_opaque [2]

entry mk_opaque_array (x: i32) = [#foo x : sum_opaque [2]]
