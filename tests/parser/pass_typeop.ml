(* Test custom operators *)
type 'a || 'b = Left of 'a | Right of 'a

type (&&) 'a 'b = And of 'a * 'b

let _ : int || int = ()

let _ : (&&) int int = ()

(* Test builtin ones *)
let _ : (->) = ()
let _ : ( *) = ()
