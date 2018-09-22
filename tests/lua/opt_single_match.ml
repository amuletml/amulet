let f = function
  | (x : ()) -> x

type z = Z

let g = function
  | (x : z) -> x

external val bottom : 'a = "nil"
let () = bottom { f, g }
