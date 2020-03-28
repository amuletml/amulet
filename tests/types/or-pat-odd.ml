type t =
  | X of int
  | Y of string

let foo = function
  | X a, Y b
  | Y b, X a -> (a, b)
  | _ -> (0, "")
