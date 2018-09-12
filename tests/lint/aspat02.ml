(* Test using as patterns on top-level lets *)
let ({ a } as foo) = { a = 0 }

let _ = { a, foo }
