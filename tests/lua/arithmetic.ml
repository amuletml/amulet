external val (+) : int -> int -> int =
  "function(x, y) return x + y end"

let main f =
  let a = f 1
  let b = f 2
  let c = (+) a
  c b

external val ignore : 'a -> () = "nil"
let () = ignore main
