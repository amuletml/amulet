(* not really a lint test, more a lower test *)

let 2 = match (1, 2) with
  | 1, 2 as foo -> foo
