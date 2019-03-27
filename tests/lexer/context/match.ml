(* Basic *)
let _ = match 1 with
| _ -> 1

(* Match bodies are blocks *)
let _ = function
| _ ->
  1
  1
| _ -> 1

(* Nesting *)
let _ = function
| _ -> function
  | _ -> 1
| _ -> 1

(* Empty function *)
let _ = function ()

(* Empty match *)
let _ =
  let _ = match x with () 2
  ( match x with (), 2 )
