let _ = function
| { x, y } -> x + y

let _ = function
| true -> ()

let f g x =
  match g x with
  | { x, y } -> x + y

let f g x =
  match g x with
  | true -> ()
