external val (^) : string -> string -> string = ""
external val (+) : int -> int -> int = ""

let main x =
  let id x = x
  let g f = f (id x) (* non-let bound variable *)
  let h f = g f
  let _ = g (fun x -> x + 1)
  h (fun x -> x ^ "")
