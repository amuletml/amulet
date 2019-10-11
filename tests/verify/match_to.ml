external val (+) : int -> int -> int = ""

let _ = function
| { x, y } -> x + y

(* Patterns with non-total patterns should not be warned. *)
let _ = function
| true -> ()

(* Don't warn when we're already of this form *)
let _ = fun { x, y } -> x + y

let f g =
  match g () with
  | { x, y } -> x + y

let f g =
  match g () with
  | true -> ()

let f g =
  let { x, y } = g ()
  x + y
