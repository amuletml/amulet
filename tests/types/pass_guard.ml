external val (>) : int -> int -> bool = ""

let foo = function
  | x when x > 2 -> x
  | _ -> 2
