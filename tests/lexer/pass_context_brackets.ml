(* We'd expect commas to terminate the function *)
let _ = (fun x -> x, 1)

(* We'd expect parenthesis to terminate the function *)
let _ = (fun x -> x)
