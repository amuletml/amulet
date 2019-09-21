let f = function
  | (x : ()) -> x

type z = Z

let g = function
  | (x : z) -> x

external val ignore : 'a -> () = "nil"
let () = ignore { f, g }
