external val (+) : int -> int -> int =
  "function(x, y) return x + y end"

let main f =
  match f () with
  | { a = a, b = b } -> a + b

external val ignore : 'a -> () = "nil"
let () = ignore main
